{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cabal-install
    ghcid
    haskell.compiler.ghc8104
  ];

  home.file = {
    ".ghc/ghci.conf".source = ./ghci.conf;
  };
}

