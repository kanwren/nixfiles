{ haskellScript
, addmeta
}:

let
  script = haskellScript {
    name = "truthtable" ;
    libraries = p: with p; [ megaparsec haskeline containers ];
    contents = ./TruthTable.hs;
  };
in addmeta script {
  description = "Generate truth tables from boolean expressions";
}

