{ pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = with pkgs; [
      ghcid
    ];

    home.file = {
      ".ghc/ghci.conf".source = ./ghci.conf;
    };

  };
}

