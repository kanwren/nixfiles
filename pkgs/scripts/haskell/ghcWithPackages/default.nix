{ writeShellScriptBin
, addmeta
}:

let
  script = writeShellScriptBin "gwp" ''
    case "$1" in
      ghc*)
        ghc="$1"
        shift
        args="$*"
        ;;
      *)
        ghc="ghc901"
        args="$*"
        ;;
    esac
    nix-shell -p "haskell.packages.$ghc.ghcWithPackages (p: with p; [ $args ])"
  '';
in
addmeta script {
  description = "Wrapper for entering a nix shell with haskell packages";
}

