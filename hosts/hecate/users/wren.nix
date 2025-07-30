{pkgs, ...}: {
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

  nix.settings.trusted-users = ["wren"];

  home-manager.users.wren = {
    home = {
      stateVersion = "25.05";

      sessionPath = ["$HOME/bin"];

      sessionVariables = {
        EDITOR = "nvim";
        VISUAL = "nvim";
      };

      packages = with pkgs; [
        # CLI/TUI stuff
        h # quick directory jumping
        tokei # count lines of code
        miniserve # serve files/directories over HTTP
        qrencode # generate QR codes

        # applications
        xfce.thunar # file browser
        bitwarden # password manager
        kdePackages.filelight # disk usage visualization
        signal-desktop # messaging
        obsidian # note taking
        libreoffice # office suite
        musescore # music composition
        qalculate-gtk # graphical calculator
        anki # flashcards
      ];
    };

    mixins.enable = true;

    programs.jujutsu.settings = {
      user.email = "nicole@wren.systems";
      signing = {
        behavior = "own";
        backend = "gpg";
        key = "002937658A2F43138C3B267E339C3A5C672CEA46";
      };
    };
  };
}
