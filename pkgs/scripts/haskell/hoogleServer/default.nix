{ writeShellScriptBin
}:

let
  script = writeShellScriptBin "hoogleserver" ''
    hoogle server --port=''${1:-8080} --local --haskell
  '';
in
addmeta script {
  description = "Start a hoogle server, usually in a nix shell";
}

