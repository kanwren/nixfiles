{ writeShellScriptBin
, substituteAll
, coreutils
, unixtools
, jq
, addmeta
}:

{
  csuncorrupt = substituteAll {
    name = "csuncorrupt";
    src = ./csuncorrupt.sh;
    inherit coreutils jq;
    dir = "bin";
    isExecutable = true;
    description = "Utility to uncorrupt CircuitSim files";
  };

  csrh = substituteAll {
    name = "csrh";
    src = ./csrh.sh;
    inherit jq;
    inherit (unixtools) getopt;
    dir = "bin";
    isExecutable = true;
    description = "Utility to view the revision history of a CircuitSim file";
  };
}

