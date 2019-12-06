{ config, pkgs, ... }:
let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "dff5f07952e61da708dc8b348ea677414e992215";
    ref = "release-19.09";
  } + "/nixos";
in

{
  imports = [
    home-manager
    # Programs to enable/configure or manage dotfiles for
    ./alacritty/default.nix
    ./tmux/default.nix
    ./zathura/default.nix
    # TODO: configure git here
  ];

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
