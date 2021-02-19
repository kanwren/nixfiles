PATH="@docker@/bin:${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }

description="Convenience script to run an autograder image"

usage_text=""
define usage_text <<'EOF'
USAGE:
    autograde [-i|--interactive] <IMAGE> [DIRECTORY]

OPTIONS:
    -i, --interactive
            Drop into an interactive shell in the running container, instead of running
            '/autograder/run_local' automatically.
    -h, --help
            Show this help text.

ARGS:
    <IMAGE>
            Autograder image to run. If the image name contains a '/', will run that image
            literally. Otherwise, will automatically detect the current semester and run the image
            'gtcs2110/<IMAGE>-<semester>:latest'.
    [DIR]
            The directory to run the autograder on, bound to /autograder/submission. Defaults to
            the current directory.

EXAMPLES
    $ autograde hw01
    $ autograde hw03 ./solution
    $ autograde gtcs2110/hw03-spring21:latest ./solution
EOF

print_usage() {
  >&2 echo "$usage_text"
}

print_help_text() {
  >&2 echo -e "$description\n\n$usage_text"
}

if [ $# -eq 0 ]; then
  print_usage
  exit 1
fi

args=$(getopt -o ih --long interactive,help -- "$@")
eval set -- "$args"

interactive="false"
for opt; do
  case "$opt" in
    -i|--interactive)
      interactive="true"
      shift
      ;;
    -h|--help)
      print_help_text
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      >&2 echo "Unknown option: $arg"
      exit 1
      ;;
  esac
done

get_semester() {
  day="$(date "+%j")"
  if [ "$day" -ge 0 ] && [ "$day" -le 120 ]; then
    season="spring"
  elif [ "$day" -ge 130 ] && [ "$day" -le 220 ]; then
    season="summer"
  elif [ "$day" -ge 230 ] && [ "$day" -le 366 ]; then
    season="fall"
  else
    return 1
  fi
  year="$(date "+%y")"
  echo "$season$year"
  return 0
}

if [[ "$1" =~ "/" ]]; then
  imageName="$1"
else
  if ! season="$(get_semester)"; then
    >&2 echo "Could not automatically determine season"
  fi
  imageName="gtcs2110/$1-spring21:latest"
fi
>&2 echo "Using $imageName"

dir="$(readlink -f "${2:-.}")"

if [ "$interactive" = "true" ]; then
  docker run --rm -it -v "$dir":/autograder/submission "$imageName" bash
else
  docker run --rm -v "$dir":/autograder/submission "$imageName" /autograder/run_local
fi

