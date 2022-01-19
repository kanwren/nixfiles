{ pkgs, lib, config, system, self, inputs, ... }:

{
  home-manager.users.nprin = {
    imports = [
      self.hmModules.xcompose

      ./bash
      ./direnv
      ./dunst
      ./firefox
      ./flameshot
      ./git
      ./gpg-agent
      ./gtk
      ./haskell
      ./kitty
      ./nushell
      ./rofi
      ./tmux
      ./vscode
      ./xserver
      ./zathura
      ./zsh
    ];

    _module.args = {
      inherit system inputs self;
    };

    home = {
      # User-specific packages. If a program needs configuration, then either:
      # - Enable and configure it via home-manager, if the option is available
      #   and configuration can be done entirely in home-manager
      # - Write the configuration file and use home-manager to manage it
      packages = with pkgs; [
        # CLI navigation
        h # quick directory jumping

        # files, formats
        tokei # count lines of code
        shellcheck # check shell scripts

        # http
        httpie # command line HTTP client
        woof # serve files locally over http
        jo # json generation

        # CLI/TUI clients over other tools
        gitAndTools.gh
        gpg-tui
        ngrok

        # development
        # Rust
        rustup
        # misc
        tectonic
        graphviz

        # system management, monitoring, and utilities
        gotop
        filelight
        xfce.thunar
        self.packages.${system}.globus-connect

        # applications
        libreoffice
        discord
        slack
        teams
        spotify
        insomnia # graphical REST API client
        anki # flashcard app
      ];
    };
  };
}
