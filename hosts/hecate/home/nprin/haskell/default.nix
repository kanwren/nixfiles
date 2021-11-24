{ pkgs, ... }:

{
  home.file = {
    ".ghc/ghci.conf".source = ./ghci.conf;
  };
}

