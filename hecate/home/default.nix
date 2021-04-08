{ pkgs, lib, config
, hmModules, inputs, nlib, ...
}:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.nprin = {
      imports = [
        hmModules.xcompose

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
        inherit inputs nlib;
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
          gist          # GitHub gists
          shellcheck    # check shell scripts
          woof          # quickly serve files locally over http
          tokei         # count lines of code
          h             # quick directory jumping
          cookiecutter  # generate projects from templates
          jo            # easy json generation for the command line
          httpie        # command-line REST API client
          insomnia      # graphical REST API client
          ranger        # console file manager
          pkgs.nur.repos.xe.pridecat

          # Development
          nodejs-14_x

          # System monitoring
          gotop
          filelight
          xfce.thunar

          # CS 2110
          cs2110.CircuitSim
          cs2110.cs2110docker
          # Complx looks terrible with anything other than the default theme, so
          # we just remove XDG_DATA_DIRS
          (runCommand "remove-complx-gtk" { buildInputs = [ makeWrapper ]; } ''
            mkdir -p "$out"/bin
            cp -r "${cs2110.complx}"/bin/* "$out"/bin
            cp -r "${cs2110.complx}"/lib "${cs2110.complx}"/share "$out"
            wrapProgram "$out"/bin/complx --unset XDG_DATA_DIRS
          '')

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
