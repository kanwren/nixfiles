{ pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "dff5f07952e61da708dc8b348ea677414e992215";
    ref = "release-19.09";
  } + "/nixos";
  utils = import ../utils { inherit lib; };
in

{
  imports =
    let
      homeProgramConfigs =
        builtins.filter builtins.pathExists
        (builtins.map (d: d + "/default.nix")
        (utils.getDirs ./.));
    in [ home-manager ] ++ homeProgramConfigs;

  home-manager.users.nprin = {
    nixpkgs.config = {
      allowUnfree = true;
    };

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
        shellcheck
        woof

        # File explorers
        ranger

        # System monitoring
        gotop
        filelight
        neofetch

        # Development
        # Java
        openjdk11
        # JavaScript
        nodejs-12_x
        nodePackages.node2nix
        # Arduino
        arduino
        ino

        # CS 2110
        cs2110.CircuitSim
        cs2110.complx-tools
        cs2110.cs2110docker

        # Documents
        libreoffice

        # Browsers
        firefox
        chromium
        qutebrowser
        unstable.torbrowser

        # Games
        fortune
        rogue

        # Jokes
        sl
        gti

        # Applications
        discord
        slack
        spotify
        musescore
        steam
        wineUnstable
      ];
    };
  };

}
