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

        ./bash
        ./direnv
        ./dunst
        ./firefox
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
        stateVersion = "21.05";

        # User-specific packages. If a program needs configuration, then either:
        # - Enable and configure it via home-manager, if the option is available
        #   and configuration can be done entirely in home-manager
        # - Write the configuration file and use home-manager to manage it
        packages = with pkgs; [
          # CLI tools
          gist # GitHub gists
          shellcheck # check shell scripts
          woof # quickly serve files locally over http
          tokei # count lines of code
          h # quick directory jumping
          cookiecutter # generate projects from templates
          jo # easy json generation for the command line
          httpie # command-line REST API client
          insomnia # graphical REST API client
          ranger # console file manager

          # Development
          stdenv.cc
          stdenv.cc.cc.man
          rustup
          nodejs-14_x

          # System monitoring
          gotop
          filelight
          xfce.thunar

          # CS 2110
          cs2110.CircuitSim
          cs2110.cs2110docker
          # Old complx looks bad with dark GTK theme
          (cs2110.complx.override { disableGTK = true; })

          # Documents
          libreoffice

          # Browsers
          firefox

          # Applications
          discord
          lightcord # from gytis' overlay
          slack
          teams
          spotify
          thunderbird
        ];
      };
    };
  };
}
