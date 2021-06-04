{ substituteAll
, runtimeShell
, xdotool
, xorg
, unixtools
, addmeta
}:

let
  script = substituteAll {
    name = "nosleep";
    src = ./nosleep.sh;
    inherit runtimeShell xdotool;
    inherit (unixtools) getopt;
    inherit (xorg) xset;
    dir = "bin";
    isExecutable = true;
  };
in
addmeta script {
  description = "Keep screen awake by moving the mouse every so often";
}

