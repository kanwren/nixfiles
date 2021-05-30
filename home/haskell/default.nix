{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cabal-install
    ghcid
    haskell.compiler.ghc901
  ];

  home.file = {
    ".ghc/ghci.conf".source = ./ghci.conf;
  };
}

