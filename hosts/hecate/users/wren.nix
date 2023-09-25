{ self }:

{ pkgs, ... }:

{
  users.users.wren = {
    initialPassword = "setup";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "audio"
      "video"
      "networkmanager"
      "docker"
      "vboxusers"
      "libvirtd"
      "wireshark"
      "dialout"
    ];
    createHome = true;
    shell = pkgs.fish;

    openssh.authorizedKeys.keys = import ../../../keys/kanwren.nix;
  };

  nix.settings.trusted-users = [ "wren" ];

  home-manager.users.wren = {
    imports = [
      self.hmModules.mixins.full
    ];

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
