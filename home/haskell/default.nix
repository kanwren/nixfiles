{ pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = with pkgs; [
      cabal-install
      ghcid
    ];

  };
}


