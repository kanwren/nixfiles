{ config, pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = with pkgs; [
      cabal-install
      ghcid
      # From summoner overlay
      haskellPackages.summoner
      haskellPackages.summoner-tui
    ];

    home.file = {
      ".summoner.toml".source = ./summoner.toml;
    };

  };
}


