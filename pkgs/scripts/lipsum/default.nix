{
  replaceVarsWith,
  runtimeShell,
  unixtools,
  curl,
  jq,
}:
let
  script = replaceVarsWith {
    name = "lipsum";
    src = ./lipsum.sh;
    dir = "bin";
    isExecutable = true;
    replacements = {
      inherit runtimeShell curl jq;
      inherit (unixtools) getopt;
    };
  };
  meta = {
    description = "Generate lorem ipsum text";
  };
in
script.overrideAttrs (old: {
  meta = (old.meta or { }) // meta;
})
