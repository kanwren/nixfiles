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
        # CLI tools
        gitAndTools.gist # GitHub gists
        gitAndTools.gh # GitHub CLI
        shellcheck # check shell scripts
        woof # quickly serve files locally over http
        tokei # count lines of code
        h # quick directory jumping
        jo # easy json generation for the command line
        httpie # command-line REST API client

        # Development
        # misc
        graphviz
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

        # Tools
        gotop
        filelight
        xfce.thunar
        custom.pkgs.globus-connect

        # CS 2110
        cs2110.CircuitSim
        cs2110.cs2110docker
        # Old complx looks bad with dark GTK theme
        (cs2110.complx.override { disableGTK = true; })

        # Applications
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
