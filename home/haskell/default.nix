{ config, pkgs, ... }:

let
  summonerHaskellPkgs = import (pkgs.fetchFromGitHub {
    owner = "nprindle";
    repo = "summoner";
    rev = "2d074a36b3e5d675f787c341e6927b8c0bb70e9a";
    sha256 = "1cf32qwsakk7fhgaa6bmyfdn70dwnnnnkfhff2pvajp4xpg8hkpd";
  }) {};
  summonerPkgs = [
    summonerHaskellPkgs.summoner summonerHaskellPkgs.summoner-tui
  ];
in
{
  home-manager.users.nprin = {

    home.packages = with pkgs; [
      cabal-install
      ghcid
    ] ++ summonerPkgs;

    home.file = {
      ".summoner.toml".source = ./summoner.toml;
    };

  };
}


