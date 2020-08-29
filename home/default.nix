{ pkgs, lib, config, ... }:

let
  sources = import ../nix/sources.nix;
  home-manager = "${sources.home-manager}/nixos";
  utils = import ../common/utils.nix { inherit lib; };
in

{
  imports = [ home-manager ];

  home-manager.users.nprin = {
    imports =
      builtins.filter builtins.pathExists
      (builtins.map (d: d + "/default.nix")
      (utils.getDirs ./.));

    # Use the system nixpkgs for home-manager. Normally, we would say:
    #   nixpkgs = { inherit (config.nixpkgs) config overlays system };
    # But this doesn't work if you pin the system nixpkgs with
    # config.nixpkgs.pkgs.
    _module.args.pkgs = config.nixpkgs.pkgs;

    home = {
      # User-specific packages. If a program needs configuration, then either:
      # - Enable and configure it via home-manager, if the option is available
      #   and configuration can be done entirely in home-manager
      # - Write the configuration file and use home-manager to manage it
      packages = with pkgs; [
        # CLI tools
        gist
        gitAndTools.hub
        shellcheck
        woof
        tokei
        h
        cookiecutter

        # Development
        nodejs-14_x
        yarn

        # System monitoring
        ytop
        filelight
        xfce.thunar

        # CS 2110
        cs2110.CircuitSim
        cs2110.cs2110docker

        # Documents
        libreoffice

        # Browsers
        firefox
        qutebrowser

        # Applications
        discord
        slack
        teams
        spotify
        spotify-tui
      ];
    };
  };

}
