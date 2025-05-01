{ replaceVarsWith
, runtimeShell
, unixtools
, gnugrep
, inkscape
}:

let
  script = replaceVarsWith {
    name = "generate-heart-emoji";
    src = ./generate_heart_emoji.sh;
    dir = "bin";
    isExecutable = true;
    replacements = {
      inherit runtimeShell gnugrep inkscape;
      inherit (unixtools) getopt;
    };
  };
  meta = {
    description = "Generate a twemoji heart with the given color";
  };
in
script.overrideAttrs (old: {
  meta = (old.meta or { }) // meta;
})

