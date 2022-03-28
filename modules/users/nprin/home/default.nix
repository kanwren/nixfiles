{ pkgs, lib, config, system, self, inputs, ... }:

{
  home-manager.users.nprin = {
    imports = [
      self.hmModules.xcompose

      ./scripts

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
      packages = with pkgs; [
        # CLI/TUI stuff
        h # quick directory jumping
        tokei # count lines of code
        shellcheck # check shell scripts
        httpie # command line HTTP client
        woof # serve files locally over http
        jo # json generation
        gotop # activity monitor

        # clients
        gitAndTools.gh
        gpg-tui
        ngrok

        # development
        rustup
        tectonic
        graphviz

        # applications
        xfce.thunar # file browser
        bitwarden # password manager
        filelight # disk usage visualization
        libreoffice
        discord
        slack
        teams
        spotify
        insomnia # graphical REST API client
        anki # flashcards

        # misc
        self.packages.${system}.globus-connect
        self.packages.${system}."scripts/nosleep"
      ];
    };
  };
}