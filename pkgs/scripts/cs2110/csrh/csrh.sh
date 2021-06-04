#! @runtimeShell@
PATH="@coreutils@/bin:@jq@/bin:@getopt@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }
join_by() { local IFS="$1"; shift; echo "$*"; }
print_help() { >&2 echo -e "$description\n\n$usage_text"; }
print_usage() { >&2 echo "$usage_text"; }

invoke_main() {
  description="Manage CircuitSim revision history"
  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh [view|check|fix|file-hash] ARGS...

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
    file-hash)
      invoke_file_hash "$@"
      ;;
    *)
      >&2 echo "Error: no such subcommand $arg"
      ;;
  esac
}

has_components() {
  test "$(jq 'has("version") and has("globalBitSize") and has("clockSpeed") and has("circuits")' "$1")" = true
}

invoke_view() {
  description="View CircuitSim revision history"
  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh view [-v|--verbose] <FILE>

OPTIONS:
    -v, --verbose
            Increase verbosity and show raw information
    -h, --help
            Show this help text

ARGS:
    <FILE>
            CircuitSim file to view
EOF

  if ! args=$(getopt -o vh --long verbose,help -n "csrh view" -- "$@"); then
    print_usage; exit 1
  fi
  eval set -- "$args"

  verbose="false"
  while :; do
    case "$1" in
      -v|--verbose) verbose="true" ;;
      -h|--help) print_help; exit 0 ;;
      --) shift; break ;;
      *) exit 1 ;;
    esac
    shift
  done

  if [ $# -eq 0 ]; then
    print_help; exit 0
  elif [ $# -ne 1 ]; then
    >&2 echo "Error: expected input file"; print_usage; exit 1
  fi

  jq -f @circuitsimViewHistoryScript@ "$1" --argjson verbose "$verbose"
}

compute_hashes() {
  # Note: requires invoke_view --verbose
  current_history="$1"
  declare -a hashes
  hash_inputs="$(echo "$current_history" | jq -r '.[] | .previous_block_hash + .file_data_hash + .timestamp_raw + (.copied_signatures | map(.start_signature, .middle_signature, .end_signature | .signature | "\t" + .) | add)' | html_encode)"
  while IFS= read -r hash_input; do
    if [ -n "$hash_input" ]; then
      hashes+=("$(echo -E -n "$hash_input" | sha256sum | awk '{print $1}')")
    fi
  done <<< "$hash_inputs"
  join_by ":" "${hashes[@]}"
}

# NOTE: CircuitSim uses strict HTML encoding/decoding in GSON, which encodes the
# following characters:
# & -> \u0026
# = -> \u003d
# ' -> \u0027
# < -> \u003c
# > -> \u003e
# jq will unescape these, so we need to fix them in the output file
html_encode() {
  sed '
    s/&/\\u0026/g;
    s/=/\\u003d/g;
    s/'"'"'/\\u0027/g;
    s/</\\u003c/g;
    s/>/\\u003e/g
  ' "$@"
}

invoke_file_hash() {
  description="Compute the hash of CircuitSim file data"

  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh file-hash <FILE>

OPTIONS:
    -h, --help
            Show this help text

ARGS:
    <FILE>
            CircuitSim file to hash
EOF

  if ! args=$(getopt -o h --long help -n "csrh file-hash" -- "$@"); then
    print_usage; exit 1
  fi
  eval set -- "$args"

  while :; do
    case "$1" in
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

  file="$1"
  # sha256 the circuit data. Note that this relies on 2-space indentation.
  # TODO: library JSON data is prepended to the circuit data. We leave it null
  # here, but it would be nice to handle it.
  file_data_hash="$(printf "%s" "null$(jq '.circuits' "$file" | html_encode)" | sha256sum | awk '{print $1}')"
  echo "$file_data_hash"
}

invoke_check() {
  description="Check the integriry of CircuitSim revision histories"

  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh check <FILE>

OPTIONS:
    -h, --help
            Show this help text

ARGS:
    <FILE>
            CircuitSim file to check
EOF

  if ! args=$(getopt -o h --long help -n "csrh check" -- "$@"); then
    print_usage; exit 1
  fi
  eval set -- "$args"

  while :; do
    case "$1" in
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

  file="$1"

  if ! has_components "$file"; then
    >&2 echo "Error: This does not look like a CircuitSim file"
    exit 1
  fi

  current_history="$(invoke_view --verbose "$file")"
  file_data_hash="$(invoke_file_hash "$file")"

  block_hashes="$(compute_hashes "$current_history")"

  if jq -f @circuitsimCheckHistoryScript@ \
      --arg hashes "$block_hashes" \
      --arg file_data_hash "$file_data_hash" \
      <(echo "$current_history"); then
    echo "OK"
  else
    exit "$?"
  fi
}

invoke_fix() {
  description="Uncorrupt CircuitSim files that are missing revision signatures"

  usage_text=""
  define usage_text <<'EOF'
USAGE:
    csrh fix [-a|--append] [-f|--force] (--timestamp TIMESTAMP) (--file-hash SHA256)
             [--random-file-hash] <FILE>

OPTIONS:
    -a, --append
            Append a new revision signature to a list of existing signatures, or creates a new
            revision signature if there are none.
    -f, --force
            Overwrite existing revision signatures with a new one.
    --timestamp TIMESTAMP
            Provide the timestamp to use in the new signature. Defaults to the current time.
            Expected format is a time in milliseconds, as given by "date +%s%3N".
    --file-hash SHA256
            Provide the file hash to use in the new signature. Defaults to the actual computed hash
            of the file data.
    --random-file-hash
            Use a random file hash in the new signature.
    -h, --help
            Show this help text

ARGS:
    <FILE>
            File to fix
EOF

  if [ $# -eq 0 ]; then
    print_help
    exit 0
  fi

  if ! args=$(getopt -o afh --long append,force,timestamp:,file-hash:,random-file-hash,help -n "csrh fix" -- "$@"); then
    print_usage
    exit 1
  fi

  eval set -- "$args"

  mode="overwrite"
  force="false"
  provided_timestamp=""
  provided_file_hash=""
  random_file_hash="false"
  while :; do
    case "$1" in
      -a|--append)
        mode="append"
        ;;
      -f|--force)
        force="true"
        ;;
      --timestamp)
        if [ -n "$provided_timestamp" ]; then
          >&2 echo "Error: timestamp already provided"
          exit 1
        fi
        shift
        if [[ ! "$1" =~ ^[0-9]+$ ]]; then
          >&2 echo "Error: provided timestamp is not valid"
          exit 1
        fi
        provided_timestamp="$1"
        ;;
      --file-hash)
        if [ -n "$provided_file_hash" ]; then
          >&2 echo "Error: file hash already provided"
          exit 1
        fi
        shift
        if [[ ! "$1" =~ ^[0-9a-f]{64}$ ]]; then
          >&2 echo "Error: provided file hash is not valid for SHA-256"
          exit 1
        fi
        provided_file_hash="$1"
        ;;
      --random-file-hash)
        if [ -n "$provided_file_hash" ]; then
          >&2 echo "Error: file hash already provided"
          exit 1
        fi
        set +e
        provided_file_hash="$(fold -w 256 /dev/urandom | head -n 1 | sha256sum | awk '{print $1}')"
        set -e
        random_file_hash="true"
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
    shift
  done

  if [ $# -eq 0 ]; then
    >&2 echo -e "Error: No files given\n"
    print_usage
    exit 1
  fi

  file="$1"
  echo "Uncorrupting $file..."

  echo "Checking file integrity..."
  if ! has_components "$file"; then
    >&2 echo "Error: This does not look like a CircuitSim file"
    exit 1
  fi

  # Get the last element .revisionSignature if present
  previous_signature="$(jq '.revisionSignatures | if (length == 0) then "" else .[-1] end' "$file" -r)"
  echo "Previous signature: $previous_signature"

  if [ -n "$previous_signature" ]; then
    # Error out if trying to overwrite an existing revision signature without -f
    if [ "$mode" != "append" ] && [ "$force" != "true" ]; then
      >&2 echo -e "Error: found previous revision signature. Check its integrity with \"csrh check\" or use --force to overwrite."
      exit 1
    fi

    # Error out if revision history is corrupted and not using -f
    if ! check_result="$(invoke_check "$file" 2>&1)"; then
      if [ "$force" = "true" ]; then
        >&2 echo "Warning: revision history is corrupted."
        >&2 echo "$check_result"
      else
        >&2 echo "Error: revision history is corrupted (run \"csrh check\" for details). Use --force to append anyway."
        exit 1
      fi
    fi
  fi

  # The previous hash is the second field of the decoded signature
  previous_hash="$(echo -n "$previous_signature" | base64 -d | cut -d$'\t' -f2)"
  echo "Previous hash: $previous_hash"

  # clear hash if overwriting with force
  if [ "$mode" != "append" ]; then
    previous_hash=""
  fi

  if [ -n "$provided_file_hash" ]; then
    file_data_hash="$provided_file_hash"
    if [ "$random_file_hash" = "true" ]; then
      echo "File data hash (random): $file_data_hash"
    else
      echo "File data hash (provided): $file_data_hash"
    fi
  else
    file_data_hash="$(invoke_file_hash "$file")"
    echo "File data hash: $file_data_hash"
  fi

  # A timestamp like outputted by System.getCurrentTimeMillis()
  if [ -n "$provided_timestamp" ]; then
    timestamp="$provided_timestamp"
    echo "Timestamp (provided): $timestamp"
  else
    timestamp="$(date "+%s%3N")"
    echo "Timestamp: $timestamp"
  fi

  # For copy-paste checking. This can be left empty.
  copied_blocks=""
  echo "Copied blocks: $copied_blocks"

  # Hashes the previous hash and current file data along with the time to get
  # the new hash
  current_hash="$(echo -n "$previous_hash$file_data_hash$timestamp$copied_blocks" | sha256sum | awk '{print $1}')"
  echo "Current hash: $current_hash"

  # The full revision signature block, before encoding
  block="$(printf "%s\t%s\t%s\t%s%s" \
    "$previous_hash" \
    "$current_hash" \
    "$timestamp" \
    "$file_data_hash" \
    "$copied_blocks")"
  echo "New revision signature block: $block"

  encoded_block="$(echo -n "$block" | base64 -w0)"
  echo "Encoded block: $encoded_block"

  if [ "$mode" = "append" ]; then
    new_json="$(jq ".revisionSignatures |= . + [\"$encoded_block\"]" "$file" --indent 2)"
  else
    new_json="$(jq ".revisionSignatures = [\"$encoded_block\"]" "$file" --indent 2)"
  fi

  new_json="$(echo -E -n "$new_json" | html_encode)"

  echo "Modifying file..."
  echo -E -n "$new_json" > "$file"
}

invoke_main "$@"
