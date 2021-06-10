{ pkgs
, lib
, config
, custom
, inputs
, ...
}:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.nprin = {
      imports = [
        custom.hmModules.xcompose

        ../../home/bash
        ../../home/direnv
        ../../home/dunst
        ../../home/firefox
        ../../home/flameshot
        ../../home/git
        ../../home/gpg-agent
        ../../home/gtk
        ../../home/haskell
        ../../home/kakoune
        ../../home/kitty
        ../../home/nushell
        ../../home/rofi
        ../../home/tmux
        ../../home/vscode
        ../../home/xserver
        ../../home/zathura
      ];

      _module.args = {
        inherit inputs custom;
      };

      home = {
        # Tries to use <nixpkgs> if version is lower then 20.09 when using flakes
        stateVersion = "21.05";

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

          # System monitoring
          gotop
          filelight
          xfce.thunar

          # CS 2110
          cs2110.CircuitSim
          cs2110.cs2110docker
          # Old complx looks bad with dark GTK theme
          (cs2110.complx.override { disableGTK = true; })

          # Applications
          firefox
          libreoffice
          lightcord # from gytis
          slack
          teams
          spotify
          insomnia # graphical REST API client
          weechat # matrix chat client
        ];
      };
    };
  };
}
