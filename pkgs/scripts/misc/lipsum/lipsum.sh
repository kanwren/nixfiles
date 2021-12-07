#! @runtimeShell@
PATH="@curl@/bin:@jq@/bin:@getopt@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }
print_help() { >&2 echo -e "$description\n\n$usage_text"; }
print_usage() { >&2 echo "$usage_text"; }

description="Generate lorem ipsum text"
usage_text=""
define usage_text <<'EOF'
USAGE:
    lipsum [-h|--help] [-s] <AMOUNT>

OPTIONS:
    -s
            Start the generated text with "Lorem ipsum".
    -h, --help
            Show this help text

ARGS:
    <AMOUNT>
            Amount of text to generate, as an integer. The single-letter suffix
            of the number indicates the units of text. Valid suffixes are "p"
            (paragraphs), "w" (words), "b" (bytes), or "l" (list items).
            Default: 5p
EOF

if [ $# -eq 0 ]; then
  print_usage
  exit 1
fi

if ! args=$(getopt -o hs --long help -n "lipsum" -- "$@"); then
  print_usage; exit 1
fi
eval set -- "$args"

start="false"
while :; do
  case "$1" in
    -h|--help)
      print_help
      exit 0
      ;;
    -s)
      start="true"
      ;;
    --)
      shift
      break
      ;;
    *)
      >&2 echo "Error: unrecognized argument: $1"
      exit 1
      ;;
  esac
  shift
done

if [ $# -gt 1 ]; then
  >&2 echo "Error: too many arguments"
  print_usage
  exit 1
fi

arg="${1:-5p}"

if [[ "$arg" =~ ^([0-9]+)([pwbl])$ ]]; then
  amount="${BASH_REMATCH[1]}"
  case "${BASH_REMATCH[2]}" in
    p) what=paras ;;
    w) what=words ;;
    b) what=bytes ;;
    l) what=lists ;;
  esac
else
  >&2 echo "Error: invalid amount"
  exit 1
fi

curl -s -X POST https://lipsum.com/feed/json -d amount="$amount" -d what="$what" -d start="$start" | jq '.feed.lipsum' -r
