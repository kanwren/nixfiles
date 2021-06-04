#! @runtimeShell@
PATH="@xdotool@/bin:@xset@/bin:@getopt@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }
join_by() { local IFS="$1"; shift; echo "$*"; }
print_help() { >&2 echo -e "$description\n\n$usage_text"; }
print_usage() { >&2 echo "$usage_text"; }

description="Keep screen from going to sleep"
usage_text=""
define usage_text <<'EOF'
USAGE:
    nosleep (-h|-x|-c SECONDS)

OPTIONS:
    -x
            Use xset to disable screen timeout
    -c SECONDS
            Use xdotool to keep the mouse moving in intervals of the given number of seconds.
    -h, --help
            Show this help text
EOF

if [ $# -eq 0 ]; then
  print_usage
  exit 1
fi

if ! args=$(getopt -o hxc: --long help -n "nosleep" -- "$@"); then
  print_usage; exit 1
fi
eval set -- "$args"

while :; do
  case "$1" in
    -h|--help)
      print_help
      exit 0
      ;;
    -x)
      xset s off
      xset -dpms
      xset s noblank
      exit 0
      ;;
    -c)
      shift
      delay="$1"
      while :; do
        xdotool mousemove_relative -- 1 0
        sleep "$delay"
        xdotool mousemove_relative -- -1 0
        sleep "$delay"
      done
      ;;
    --)
      break
      ;;
    *)
      >&2 echo "Error: unrecognized argument: $1"
      exit 1
      ;;
  esac
  shift
done

if [ $# -gt 0 ]; then
  >&2 echo "Error: invalid arguments"
  print_usage
  exit 1
fi
