{ nord-tmux }:

{ pkgs, lib, config, ... }:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.nprin = {
      imports = [
        ./bash
        ./dunst
        ./firefox
        ./git
        ./gtk
        ./haskell
        ./kitty
        ./nushell
        (import ./tmux { inherit nord-tmux; })
        ./zathura
        ./direnv
        ./rofi
      ];

      home = {
        # Tries to use <nixpkgs> if version is lower when using flakes
        stateVersion = "20.09";

        # User-specific packages. If a program needs configuration, then either:
        # - Enable and configure it via home-manager, if the option is available
        #   and configuration can be done entirely in home-manager
        # - Write the configuration file and use home-manager to manage it
        packages = with pkgs; [
          # CLI tools
          gist
          shellcheck
          woof
          tokei
          h
          cookiecutter

          # Development
          nodejs-14_x

          # System monitoring
          gotop
          filelight
          xfce.thunar

          # CS 2110
          cs2110.CircuitSim
          cs2110.cs2110docker

          # Documents
          libreoffice

          # Browsers
          firefox

          # Applications
          discord
          slack
          teams
          spotify
        ];
      };
    };
  };
}
