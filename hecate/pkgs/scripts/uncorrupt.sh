PATH="@coreutils@/bin:@jq@/bin:${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }

description="Uncorrupt CircuitSim files that are missing revision signatures"

usage_text=""
define usage_text <<'EOF'
USAGE:
    uncorrupt [-a|--append] [-f|--force] <FILE>...

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
            File(s) to uncorrupt
EOF

print_help() {
  >&2 echo -e "$description\n\n$usage_text"
}

print_usage() {
  >&2 echo "$usage_text"
}

if [ $# -eq 0 ]; then
  print_help
  exit 0
fi

args=$(getopt -o afh --long append,force,help -- "$@")
eval set -- "$args"

if [ "$1" = "-a" ] || [ "$1" = "--append" ]; then
  mode="append"
  shift
else
  mode="overwrite"
fi

mode="overwrite"
for opt; do
  case "$opt" in
    -a|--append)
      mode="append"
      shift
      ;;
    -f|--force)
      mode="force"
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
    *)
      >&2 echo "Unknown option: $opt"
      print_usage
      exit 1
      ;;
  esac
done

if [ "$#" -eq 0 ]; then
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
  previous_hash="$(echo -n "$previous_signature" | base64 -d | cut -d'	' -f2)"
  echo "Previous hash: $previous_hash"

  # If there's already a revision signature, don't overwrite unless passing -f
  if [ -n "$previous_hash" ] && [ "$mode" != "append" ]; then
    if [ "$mode" = "force" ]; then
      previous_hash=""
    else
      >&2 echo -e "Error: found previous revision signature. Use --force to overwrite."
      continue
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
