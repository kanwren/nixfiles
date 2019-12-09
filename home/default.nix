{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "dff5f07952e61da708dc8b348ea677414e992215";
    ref = "release-19.09";
  } + "/nixos";
in

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
        # Development
        openjdk11
        nodejs
        nodePackages.typescript
        ghcid

        # CLI tools
        gist
        gitAndTools.hub
        jq
        cloc

        # Images
        ffmpeg
        scrot
        imagemagick
        gimp

        # Applications
        unstable.discord
        unstable.slack
        spotify
        firefox
      ];
    };
  };

}
