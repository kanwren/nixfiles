{ pkgs, config, ... }:

{
  imports = [
    ./picom.nix
    ./redshift.nix
  ];

  services.logind = {
    extraConfig = ''
      IdleAction=ignore
      HandlePowerKey=ignore
    '';
  };

  services.xserver = {
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3;

      configFile = pkgs.substituteAll {
        src = ./i3config/i3config;

        spill_container_script = pkgs.substituteAll {
          src = ./i3config/spill_container.sh;
          isExecutable = true;
          inherit (pkgs) runtimeShell jq;
          i3 = config.services.xserver.windowManager.i3.package;
        };

        pick_game_script = pkgs.substituteAll {
          src = ./i3config/pick_game.sh;
          isExecutable = true;
          inherit (pkgs) rofi;
        };

        i3status_config = pkgs.writeText "i3status.conf" ''
          ${builtins.readFile "${pkgs.i3status}/etc/i3status.conf"}

          general {
            color_good = "#a6e3a1"
            color_degraded = "#f9e2af"
            color_bad = "#f38ba8"
          }
        '';
      };

      extraPackages = with pkgs; [
        # Menu/launcher
        rofi
        # Status bar
        i3status
        # Lock screen
        i3lock
        betterlockscreen
        # Clipboard manager
        copyq
        # Applets
        networkmanagerapplet
        blueman
        mate.mate-media
        # Media controls (pactl is provided by pulseaudio)
        playerctl
      ];
    };
  };

  programs = {
    nm-applet.enable = true;
  };
}
