{ pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "dff5f07952e61da708dc8b348ea677414e992215";
    ref = "release-19.09";
  } + "/nixos";
  utils = import ../utils { inherit lib; };
in

with rec {
  sherlock = import (pkgs.fetchFromGitHub {
    owner = "nprindle";
    repo = "sherlock";
    rev = "8ff5f358b3502e6d88d05d9fdb6c5d33e64fdd1f";
    sha256 = "1sndy0lkw4f16mxb6z3liz0mfbp0w679nbv6m8q3chd37zas8206";
  }) { enableTor = true; };
};

{
  imports =
    let
      homeProgramConfigs =
        builtins.filter builtins.pathExists
        (builtins.map (d: d + "/default.nix")
        (utils.getDirs ./.));
    in [ home-manager ] ++ homeProgramConfigs;

  home-manager.users.nprin = {
    home = {
      # User-specific packages. If a program needs configuration, then either:
      # - Enable and configure it via home-manager, if the option is available
      #   and configuration can be done entirely in home-manager
      # - Write the configuration file and use home-manager to manage it
      packages = with pkgs; [
        # Basic CLI tools
        ag
        tldr
        jq
        cloc
        gist
        gitAndTools.hub

        # System monitoring
        gotop
        filelight
        neofetch

        # Development
        openjdk11
        nodejs-12_x
        nodePackages.node2nix

        # CS 2110
        cs2110.CircuitSim
        cs2110.complx-tools
        cs2110.cs2110docker

        # OSINT
        sherlock

        # Documents
        libreoffice

        # Browsers
        firefox
        chromium
        unstable.torbrowser

        # Applications
        discord
        slack
        spotify
        musescore
        #unstable.steam
        wineUnstable
      ];
    };
  };

}
