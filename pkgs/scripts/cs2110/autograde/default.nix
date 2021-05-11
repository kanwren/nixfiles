{ substituteAll
, runtimeShell
, docker
}:

substituteAll {
  name = "autograde";
  src = ./autograde.sh;
  inherit runtimeShell docker;
  dir = "bin";
  isExecutable = true;
  description = "Convenience script to wrap 'docker run' for running CS2110 autograders";
}

