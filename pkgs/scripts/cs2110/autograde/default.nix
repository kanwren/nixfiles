{ substituteAll
, runtimeShell
, docker
, addmeta
}:

let
  script = substituteAll {
    name = "autograde";
    src = ./autograde.sh;
    inherit runtimeShell docker;
    dir = "bin";
    isExecutable = true;
  };
in
addmeta script {
  description = "Convenience script to wrap 'docker run' for running CS2110 autograders";
}

