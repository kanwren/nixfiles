{ haskellScript
, addmeta
}:

let
  script = haskellScript {
    name = "serve";
    libraries = p: with p; [ warp wai-extra wai-middleware-static lucid optparse-applicative ];
    contents = ./Serve.hs;
  };
in addmeta script {
  description = "Serve directories/files over HTTP";
}

