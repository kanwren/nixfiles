{ writeShellScriptBin
, substituteAll
, runtimeShell
, coreutils
, unixtools
, jq
}:

substituteAll {
  name = "csrh";
  src = ./csrh.sh;
  inherit runtimeShell coreutils jq;
  inherit (unixtools) getopt;
  circuitsimViewHistoryScript = ./circuitsim-view-history.jq;
  circuitsimCheckHistoryScript = ./circuitsim-check-history.jq;
  dir = "bin";
  isExecutable = true;
  description = "Utility to manage the revision history of a CircuitSim file";
}

