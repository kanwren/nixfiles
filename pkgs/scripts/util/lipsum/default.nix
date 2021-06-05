{ substituteAll
, runtimeShell
, unixtools
, curl
, jq
, addmeta
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
in
addmeta script {
  description = "Generate lorem ipsum text";
}

