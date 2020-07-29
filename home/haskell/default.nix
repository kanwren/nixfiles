{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cabal-install
    ghcid
    haskell.compiler.ghc8101
  ];

  home.file = {
    ".ghc/ghci.conf".source = ./ghci.conf;
  };
}

