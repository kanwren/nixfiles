{ substituteAll
, docker
}:

substituteAll {
  name = "autograde";
  src = ./autograde.sh;
  inherit docker;
  dir = "bin";
  isExecutable = true;
  description = "Convenience script to wrap 'docker run' for running CS2110 autograders";
}

