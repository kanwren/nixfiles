self:

{ pkgs, lib, ... }:

{
  home-manager.users.wren = {
    imports = [
      self.hmModules.spicetify

      ./bash
      ./btop
      ./cava
      ./direnv
      ./discord
      ./dunst
      ./firefox
      ./flameshot
      ./git
      ./gpg-agent
      ./gtk
      ./haskell
      ./kitty
      ./nix
      ./rofi
      ./rust
      ./spotify
      ./tmux
      ./vscode
      ./zathura
      ./zsh
    ];

    _module.args = { inherit self; };

    home = {
      stateVersion = "22.11";

      sessionPath = [ "$HOME/bin" ];

      packages = with pkgs; [
        # CLI/TUI stuff
        h # quick directory jumping
        tokei # count lines of code
        shellcheck # check shell scripts
        btop # activity monitor
        gitAndTools.gh # github client
        jo # json generation
        httpie # command line HTTP client
        miniserve # serve files/directories over HTTP
        ngrok # expose tunnels to local servers
        qrencode # generate QR codes

        # development
        tectonic
        graphviz

        # applications
        xfce.thunar # file browser
        bitwarden # password manager
        filelight # disk usage visualization
        libreoffice
        slack
        teams
        insomnia # graphical REST API client
        anki # flashcards
      ];
    };
  };
}
