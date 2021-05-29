{ writeShellScriptBin
, substituteAll
, runtimeShell
, coreutils
, unixtools
, jq
, addmeta
}:

let
  script = substituteAll {
    name = "csrh";
    src = ./csrh.sh;
    inherit runtimeShell coreutils jq;
    inherit (unixtools) getopt;
    circuitsimViewHistoryScript = ./circuitsim-view-history.jq;
    circuitsimCheckHistoryScript = ./circuitsim-check-history.jq;
    dir = "bin";
    isExecutable = true;
  };
in
addmeta script {
  description = "Utility to manage the revision history of a CircuitSim file";
}

