#! @runtimeShell@
PATH="@coreutils@/bin:@jq@/bin:@getopt@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }
print_help() { >&2 echo -e "$description\n\n$usage_text"; }
print_usage() { >&2 echo "$usage_text"; }

invoke_main() {
  description="Manage CircuitSim revision history"
  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh [view|check|fix] ARGS...

OPTIONS:
    -h, --help
            Show this help text
EOF

  if [ $# -eq 0 ]; then
    print_usage
    exit 1
  fi

  arg="$1"
  shift

  case "$arg" in
    -h|--help)
      print_help
      exit 0
      ;;
    view)
      invoke_view "$@"
      ;;
    check)
      invoke_check "$@"
      ;;
    fix)
      invoke_fix "$@"
      ;;
    *)
      >&2 echo "Error: no such subcommand $arg"
      ;;
  esac
}

invoke_check() {
  >&2 echo "Error: unimplemented"
  exit 1
}

invoke_view() {
  description="View CircuitSim revision history"
  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh view <FILE>

OPTIONS:
    -h, --help
            Show this help text

ARGS:
    <FILE>
            CircuitSim file to view
EOF

  if ! args=$(getopt -o h --long help -n "csrh view" -- "$@"); then
    print_usage; exit 1
  fi
  eval set -- "$args"

  for opt; do
    case "$opt" in
      -h|--help) print_help; exit 0 ;;
      --) shift; break ;;
      *) exit 1 ;;
    esac
  done

  if [ $# -eq 0 ]; then
    print_help; exit 0
  elif [ $# -ne 1 ]; then
    >&2 echo "Error: expected input file"; print_usage; exit 1
  fi

  jq -f @circuitsimHistoryScript@ "$1"
}

invoke_fix() {
  description="Uncorrupt CircuitSim files that are missing revision signatures"

  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh fix [-a|--append] [-f|--force] <FILE>...

OPTIONS:
    -a, --append
            Append a new revision signature to a list of existing signatures, or creates a new
            revision signature if there are none.
    -f, --force
            Overwrite existing revision signatures with a new one.
    -h, --help
            Show this help text

ARGS:
    <FILE>...
            File(s) to fix
EOF

  if [ $# -eq 0 ]; then
    print_help
    exit 0
  fi

  if ! args=$(getopt -o afh --long append,force,help -n "csrh fix" -- "$@"); then
    print_usage
    exit 1
  fi

  eval set -- "$args"

  mode="overwrite"
  force="false"
  for opt; do
    case "$opt" in
      -a|--append)
        checkmode "append"
        mode="append"
        shift
        ;;
      -f|--force)
        force="true"
        shift
        ;;
      -h|--help)
        print_help
        exit 0
        ;;
      --)
        shift
        break
        ;;
      *) exit 1 ;;
    esac
  done

  if [ $# -eq 0 ]; then
    >&2 echo -e "Error: No files given\n"
    print_usage
    exit 1
  fi

  for ((n=1; n <= $#; n += 1)); do
    if [ "$n" -gt 1 ]; then
      echo ""
    fi

    file="${!n}"
    echo "Uncorrupting $file..."

    echo "Checking file integrity..."
    if [ "$(jq 'has("version") and has("globalBitSize") and has("clockSpeed") and has("circuits")' "$file")" != "true" ]; then
      >&2 echo "Error: This does not look like a CircuitSim file"
      continue
    fi

    # Get the last element .revisionSignature if present
    previous_signature="$(jq '.revisionSignatures | if (length == 0) then "" else .[-1] end' "$file" -r)"
    echo "Previous signature: $previous_signature"

    # The previous hash is the second field of the decoded signature
    previous_hash="$(echo -n "$previous_signature" | base64 -d | cut -d$'\t' -f2)"
    echo "Previous hash: $previous_hash"

    discarded_hash=""
    if [ -n "$previous_hash" ] && [ "$mode" != "append" ]; then
      # Error out if trying to overwrite an existing revision signature without -f
      if [ "$mode" != "append" ]; then
        if [ "$force" = "true" ]; then
          discarded_hash="$previous_hash"
          previous_hash=""
        else
          >&2 echo -e "Error: found previous revision signature. Check its integrity with \"csrh check\" or use --force to overwrite."
          continue
        fi
      fi

      # Error out if revision history is corrupted and not using -f
      if ! check_result="$(invoke_check "$file")"; then
        if [ "$force" = "true" ]; then
          >&2 echo "Warning: revision history is corrupted."
          >&2 echo "$check_result"
        else
          >&2 echo "Error: revision history is corrupted (run \"csrh check\" for details). Use --force to append anyway."
          continue
        fi
      fi
    fi

    # sha256 the circuit data. Note that this relies on 2-space indentation.
    # TODO: library JSON data is prepended to the circuit data. We leave it null
    # here, but it would be nice to handle it.
    file_data_hash="$(printf "%s" "null$(jq '.circuits' "$file")" | sha256sum | awk '{print $1}')"
    echo "File data hash: $file_data_hash"

    # A timestamp like outputted by System.getCurrentTimeMillis()
    timestamp="$(date "+%s%3N")"
    echo "Timestamp: $timestamp"

    # For copy-paste checking. This can be left empty.
    copied_blocks=""
    echo "Copied blocks: $copied_blocks"

    # Hashes the previous hash and current file data along with the time to get
    # the new hash
    current_hash="$(echo -n "$previous_hash$file_data_hash$timestamp" | sha256sum | awk '{print $1}')"
    echo "Current hash: $current_hash"

    # The full revision signature block, before encoding
    block="$(printf "%s\t%s\t%s\t%s%s" \
      "$previous_hash" \
      "$current_hash" \
      "$timestamp" \
      "$file_data_hash" \
      "$copied_blocks")"
    echo "New revision signature block: $block"

    encoded_block="$(echo -n "$block" | base64 -w0 | sed 's/=/\\u003d/g')"
    echo "Encoded block: $encoded_block"

    if [ "$mode" = "append" ]; then
      new_json="$(jq ".revisionSignatures |= . + [\"$encoded_block\"]" "$file" --indent 2)"
    else
      new_json="$(jq ".revisionSignatures = [\"$encoded_block\"]" "$file" --indent 2)"
    fi

    echo "Modifying file..."
    echo "$new_json" > "$file"
  done
}

invoke_main "$@"
