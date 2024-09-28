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
  };

  nix.settings.trusted-users = [ "wren" ];

  home-manager.users.wren = {
    home = {
      stateVersion = "22.11";

      sessionPath = [ "$HOME/bin" ];

      packages = with pkgs; [
        # CLI/TUI stuff
        h # quick directory jumping
        tokei # count lines of code
        miniserve # serve files/directories over HTTP
        # ngrok # expose tunnels to local servers
        qrencode # generate QR codes

        # applications
        xfce.thunar # file browser
        bitwarden # password manager
        filelight # disk usage visualization
        libreoffice
        anki # flashcards
      ];
    };

    mixins.enable = true;

    programs.jujutsu.settings = {
      user.email = "nicole@wren.systems";
      signing = {
        sign-all = true;
        backend = "gpg";
        key = "07B776B0672AF6A4CF629919F1A41BE43E4EAA99";
      };
    };
  };
}
