{ writeShellScriptBin
, substituteAll
, runtimeShell
, coreutils
, unixtools
, jq
, addmeta
}:

{
  csuncorrupt = substituteAll {
    name = "csuncorrupt";
    src = ./csuncorrupt.sh;
    inherit runtimeShell coreutils jq;
    dir = "bin";
    isExecutable = true;
    description = "Utility to uncorrupt CircuitSim files";
  };

  csrh = substituteAll {
    name = "csrh";
    src = ./csrh.sh;
    inherit runtimeShell jq;
    inherit (unixtools) getopt;
    dir = "bin";
    isExecutable = true;
    description = "Utility to view the revision history of a CircuitSim file";
  };
}

