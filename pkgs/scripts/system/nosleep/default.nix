{ writeShellScriptBin, xdotool
, addmeta
}:

let
  script = writeShellScriptBin "nosleep" ''
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
      >&2 echo "Usage: nosleep [seconds]"
      exit
    fi

    delay="''${1:-59}"

    while :; do
      ${xdotool}/bin/xdotool mousemove_relative -- 1 0
      sleep "$delay"
      ${xdotool}/bin/xdotool mousemove_relative -- -1 0
      sleep "$delay"
    done
  '';
in addmeta script {
  description = "Keep screen awake by moving the mouse every so often";
}

