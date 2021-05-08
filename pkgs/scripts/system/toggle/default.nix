{ writeShellScriptBin
, addmeta
}:

let
  script = writeShellScriptBin "toggle" ''
    if [ $# -ne 1 ]; then
      >&2 echo "Usage: toggle PROGRAM"
      exit 1
    fi
    progname="$1"
    status="$(systemctl --user is-active "$progname")"
    case "$status" in
      active)
        systemctl --user stop "$progname"
        echo "$progname has been toggled off"
        ;;
      inactive)
        systemctl --user restart "$progname"
        echo "$progname has been toggled on"
        ;;
      *)
        >&2 echo "error: status is $status"
        exit 1
        ;;
    esac
  '';
in addmeta script {
  description = "Brute-force toggling of systemd services";
}

