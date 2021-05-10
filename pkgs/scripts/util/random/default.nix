{ haskellScript
, addmeta
}:

let
  script = haskellScript {
    name = "random";
    libraries = p: with p; [ splitmix optparse-applicative ];
    contents = ./Random.hs;
  };
in
addmeta script {
  description = "Simple CLI random number generator";
}

