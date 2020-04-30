{ pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = with pkgs; [
      cabal-install
      ghcid
    ];

    home.file = {
      ".ghc/ghci.conf".source = ./ghci.conf;
    };

  };
}


