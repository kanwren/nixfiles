{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "dff5f07952e61da708dc8b348ea677414e992215";
    ref = "release-19.09";
  } + "/nixos";
in

with rec {
  sherlock = import (pkgs.fetchFromGitHub {
    owner = "nprindle";
    repo = "sherlock";
    rev = "e7125a9cf9d4a95abc19c7046965fdb2e13ea62e";
    sha256 = "1nabjp6hdajqvh6b58cbns2y94dr12a4wpjlj3bvbyc7q6ydpyk8";
  }) { enableTor = true; };
};

{
  imports =
    let
      # Get all subdirectories in a directory
      getDirs = dir:
        builtins.map (x: dir + "/${x}")
        (builtins.attrNames
        (lib.filterAttrs (_: type: type == "directory")
        (builtins.readDir dir)));
      homeProgramConfigs =
        builtins.filter builtins.pathExists
        (builtins.map (d: d + "/default.nix")
        (getDirs ./.));
    in [ home-manager ] ++ homeProgramConfigs;

  home-manager.users.nprin = {
    home = {
      # User-specific packages. If a program needs configuration, then either:
      # - Enable and configure it via home-manager, if the option is available
      #   and configuration can be done entirely in home-manager
      # - Write the configuration file and use home-manager to manage it
      packages = with pkgs; [
        # CLI tools
        ag
        tldr
        jq
        cloc
        exiftool
        gist
        gitAndTools.hub

        # System monitoring
        gotop
        filelight
        neofetch

        # Development
        openjdk11
        nodejs

        # CS 2110
        cs2110.complx-tools

        # OSINT
        sherlock

        # Plaintext accounting
        haskellPackages.hledger

        # Media
        ffmpeg
        vlc
        scrot
        imagemagick
        gimp
        inkscape

        # Documents
        libreoffice

        # Browsers
        firefox
        chromium
        unstable.torbrowser

        # Applications
        unstable.discord
        unstable.slack
        spotify
        musescore
        unstable.steam
        unstable.wineUnstable
      ];
    };
  };

}
