{ substituteAll
, runtimeShell
, unixtools
, gnugrep
, inkscape
, addmeta
}:

let
  script = substituteAll {
    name = "generate-heart-emoji";
    src = ./generate_heart_emoji.sh;
    inherit runtimeShell gnugrep inkscape;
    inherit (unixtools) getopt;
    dir = "bin";
    isExecutable = true;
  };
in
addmeta script {
  description = "Generate a twemoji heart with the given color";
}

