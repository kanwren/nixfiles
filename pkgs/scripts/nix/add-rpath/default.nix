{ writeShellScriptBin, patchelf
, addmeta
}:

let
  script = writeShellScriptBin "add-rpath" ''
    set -euo pipefail

    print_usage() { >&2 echo "Usage: add-rpath [-a|--append] <path> <files...>"; }
    if ! args=$(getopt -o ah --long append,help -n csrh -- "$@"); then
      print_usage; exit 1
    fi
    eval set -- "$args"

    appendType="prepend"
    for opt; do
      case "$opt" in
        -h|--help) print_usage; exit 0 ;;
        -a|--append) appendType="append"; shift; break ;;
        --) shift; break ;;
        *) exit 1 ;;
      esac
    done

    if [ $# -lt 2 ]; then
      print_usage; exit 1
    fi

    path="$1"
    shift
    for f in "$@"; do
      cur_rpath="$(patchelf --print-rpath "$f")"
      if [ "$appendType" = "prepend" ]; then
        new_rpath="$path:$cur_rpath"
      elif [ "$appendType" = "append" ]; then
        new_rpath="$cur_rpath:$path"
      fi
      ${patchelf}/bin/patchelf --set-rpath "$new_rpath" "$f"
    done
  '';
in addmeta script {
  description = "Append a path to the rpath of an executable";
}

