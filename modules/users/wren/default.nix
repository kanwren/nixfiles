toplevel:

let
  userInfo = {
    name = "Nicole Wren";
    email = "nicole@wren.systems";
  };
in
{
  flake.modules.nixos."users/wren" =
    { pkgs, ... }:
    {
      users.users.wren = {
        isNormalUser = true;
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
        imports = [ toplevel.config.flake.modules.homeManager."users/wren" ];
      };
    };

  flake.modules.homeManager."users/wren" =
    { pkgs, config, ... }:
    {
      imports = [
        toplevel.config.flake.modules.homeManager.btop
        toplevel.config.flake.modules.homeManager.catppuccin
        toplevel.config.flake.modules.homeManager.discord
        toplevel.config.flake.modules.homeManager.gaming
        toplevel.config.flake.modules.homeManager.ghostty
        toplevel.config.flake.modules.homeManager.git
        toplevel.config.flake.modules.homeManager.go
        toplevel.config.flake.modules.homeManager.gpg
        toplevel.config.flake.modules.homeManager.jujutsu
        toplevel.config.flake.modules.homeManager.noctalia
        toplevel.config.flake.modules.homeManager.shell
        toplevel.config.flake.modules.homeManager.zathura
        toplevel.config.flake.modules.homeManager.zellij
      ];

      home = {
        stateVersion = "25.05";

        sessionPath = [ "$HOME/bin" ];

        sessionVariables = {
          EDITOR = "nvim";
          VISUAL = "nvim";
          GPGKEY = userInfo.email;
        };

        packages = [
          pkgs.bitwarden-desktop
          pkgs.mkvtoolnix
          pkgs.python314Packages.subliminal
          pkgs.qalculate-gtk
          pkgs.signal-desktop
          pkgs.transmission_4-gtk
          pkgs.yt-dlp
        ];
      };

      programs = {
        git.settings = {
          user = userInfo;
          signing = {
            signByDefault = true;
            key = "002937658A2F43138C3B267E339C3A5C672CEA46";
            format = "openpgp";
          };
        };

        jujutsu.settings = {
          user = userInfo;
          signing = {
            behavior = "own";
            backend = "gpg";
            key = "002937658A2F43138C3B267E339C3A5C672CEA46";
          };
          templates.git_push_bookmark = ''"kanwren/push-" ++ change_id.short()'';
        };

        chromium.enable = true;

        firefox = {
          enable = true;
          configPath = "${config.xdg.configHome}/mozilla/firefox";
        };

        obsidian.enable = true;

        rclone.enable = true;

        yt-dlp.enable = true;
      };
    };
}
