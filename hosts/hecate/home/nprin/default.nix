{ pkgs
, lib
, config
, custom
, inputs
, ...
}:

{
  home-manager.users.nprin = {
    imports = [
      custom.hmModules.xcompose

      ./bash
      ./direnv
      ./dunst
      ./firefox
      ./flameshot
      ./git
      ./gpg-agent
      ./gtk
      ./haskell
      ./kakoune
      ./kitty
      ./nushell
      ./rofi
      ./tmux
      ./vscode
      ./xserver
      ./zathura
    ];

    _module.args = {
      inherit inputs custom;
    };

    home = {
      # Tries to use <nixpkgs> if version is lower then 20.09 when using flakes
      stateVersion = "21.11";

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

        # development
        # C
        stdenv.cc
        stdenv.cc.cc.man
        # Rust
        rustup
        cargo-cache
        cargo-edit
        cargo-expand
        cargo-feature
        cargo-inspect
        cargo-valgrind
        cargo-watch
        # misc
        tectonic
        graphviz

        # system management, monitoring, and utilities
        gotop
        filelight
        xfce.thunar
        custom.pkgs.globus-connect

        # applications
        firefox
        libreoffice
        discord
        slack
        teams
        spotify
        insomnia # graphical REST API client
        weechat # matrix chat client
        anki # flashcard app
      ];
    };
  };
}
