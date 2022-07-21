#! @runtimeShell@
PATH="@inkscape@/bin:@gnugrep@/bin:@getopt@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }
print_help() { >&2 echo -e "$description\n\n$usage_text"; }
print_usage() { >&2 echo "$usage_text"; }

description="Generate a twemoji heart with the given color"
usage_text=""
define usage_text <<'EOF'
USAGE:
    generate-heart-emoji [-h|--help] [(-f|--format) FORMAT] [(-o|--outfile) FILENAME] <COLOR>

OPTIONS:
    -h, --help
            Show this help text
    -f, --format (png|svg)
            Output in the provided format. Defaults to SVG.
    -o, --outfile FILENAME
            The file to output the image to. If no file is
            provided, defaults to standard output.

ARGS:
    <COLOR>
            A CSS color.
            Examples: #b4befe, rgb(148, 226, 213), LavenderBlush
EOF

if [ $# -eq 0 ]; then
  print_help
  exit 1
fi

if ! args=$(getopt --options hf:o: --longoptions help,format:,outfile: -n "generate-heart-emoji" -- "$@"); then
  print_usage; exit 1
fi
eval set -- "$args"

output_to_file=0
fmt="auto"
while :; do
  case "$1" in
    -h|--help)
      print_help
      exit 0
      ;;
    -f|--format)
      fmt="$(echo "$2" | tr '[:upper:]' '[:lower:]')"
      if [ "$fmt" != "svg" ] && [ "$fmt" != "png" ]; then
        >&2 echo "Error: unrecognized output format: $fmt"
        >&2 echo "Valid output formats: svg, png"
        exit 1
      fi
      shift
      ;;
    -o|--outfile)
      outfile="$2"
      output_to_file=1
      shift
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

if [ $# -lt 1 ]; then
  >&2 echo "Error: expected heart color"
  print_usage
  exit 1
fi

if [ $# -gt 1 ]; then
  >&2 echo "Error: too many arguments"
  print_usage
  exit 1
fi

if [ "$fmt" = "auto" ]; then
  if [ "$output_to_file" = 0 ]; then
    fmt="svg"
  elif echo "$outfile" | grep -qi "\.png$"; then
    fmt="png"
  elif echo "$outfile" | grep -qi "\.svg$"; then
    fmt="svg"
  else
    >&2 echo "Error: could not detect output image format; specify it with --format"
    exit 1
  fi
fi

color="$1"

svg_text=""
define svg_text <<EOF
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 36 36">
  <path fill="$color" d="M35.885 11.833c0-5.45-4.418-9.868-9.867-9.868-3.308 0-6.227 1.633-8.018 4.129-1.791-2.496-4.71-4.129-8.017-4.129-5.45 0-9.868 4.417-9.868 9.868 0 .772.098 1.52.266 2.241C1.751 22.587 11.216 31.568 18 34.034c6.783-2.466 16.249-11.447 17.617-19.959.17-.721.268-1.469.268-2.242z"/>
</svg>
EOF

if [ "$fmt" = "png" ]; then
  if [ "$output_to_file" = 0 ]; then
    outfile="-"
  fi
  inkscape -w 218 -h 218 --export-type=png <(echo "$svg_text") -o "$outfile"
else
  if [ "$output_to_file" = 0 ]; then
    echo "$svg_text"
  else
    echo "$svg_text" > "$outfile"
  fi
fi

