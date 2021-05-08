{ writeShellScriptBin
, addmeta
}:

let
  script = writeShellScriptBin "bak" ''
    if [ $# -ge 1 ]; then
      if [ "$1" = "-u" ]; then
        shift
        for arg in "$@"; do
          if [ -f "$arg" ]; then
            case "$arg" in
              *.bak)
                mv "$arg" "''${arg%.*}"
                echo "Moved $arg to ''${arg%.*}"
                ;;
              *)
                >&2 echo "Error: $arg is not a .bak file"
                ;;
            esac
          else
             >&2 echo "Error: cannot find file $arg"
          fi
        done
      else
        for arg in "$@"; do
          if [ -f "$arg" ]; then
            cp "$arg" "$arg.bak"
            echo "Copied $arg to $arg.bak"
          else
            >&2 echo "Error: cannot find file $arg"
          fi
        done
      fi
    else
      >&2 echo "Usage: bak [-u] <file1> [ <file2>...]"
      exit 1
    fi
  '';
in addmeta script {
  description = "Copy a file to make a backup";
}


