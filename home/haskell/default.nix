{ pkgs, ... }:

{
  home.packages = with pkgs; [
    cabal-install
    ghcid
    haskell.compiler.ghc8104
    stylish-haskell
  ];

  home.file = {
    ".ghc/ghci.conf".source = ./ghci.conf;
  };
}

