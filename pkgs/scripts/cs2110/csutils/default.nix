{ writeShellScriptBin, substituteAll
, coreutils, jq
, addmeta
}:

{
  csuncorrupt = substituteAll {
    name = "csuncorrupt";
    src = ./csuncorrupt.sh;
    inherit coreutils jq;
    dir = "bin";
    isExecutable = true;
    description = "Utility to uncorrupt CircuitSim files";
  };

  csrh =
    let
      script = writeShellScriptBin "csrh" ''
        set -euo pipefail

        define() { IFS=$'\n' read -r -d ''' "''${1}" || true; }
        description="View CircuitSim revision history"
        usage_text=""
        define usage_text <<'EOF'
        USAGE:
            csrh <FILE>

        OPTIONS:
            -h, --help
                    Show this help text

        ARGS:
            <FILE>
                    CircuitSim file to view
        EOF

        print_help() { >&2 echo -e "$description\n\n$usage_text"; }
        print_usage() { >&2 echo "$usage_text"; }

        if ! args=$(getopt -o h --long help -n csrh -- "$@"); then
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

        "${jq}/bin/jq" -f "${./circuitsim-history.jq}" "$1"
      '';
    in addmeta script {
      description = "Utility to view the revision history of a CircuitSim file";
    };
}

