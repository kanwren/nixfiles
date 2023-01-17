{ pkgs, config, ... }:

{
  imports = [
    ../base
    ./picom.nix
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
