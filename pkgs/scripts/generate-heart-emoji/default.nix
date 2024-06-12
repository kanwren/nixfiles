{ substituteAll
, runtimeShell
, unixtools
, gnugrep
, inkscape
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
  meta = {
    description = "Generate a twemoji heart with the given color";
  };
in
script.overrideAttrs (old: {
  meta = (old.meta or { }) // meta;
})

