{ substituteAll
, runtimeShell
, unixtools
, curl
, jq
}:

let
  script = substituteAll {
    name = "lipsum";
    src = ./lipsum.sh;
    inherit runtimeShell curl jq;
    inherit (unixtools) getopt;
    dir = "bin";
    isExecutable = true;
  };
  meta = {
    description = "Generate lorem ipsum text";
  };
in
script.overrideAttrs (old: {
  meta = (old.meta or { }) // meta;
})
