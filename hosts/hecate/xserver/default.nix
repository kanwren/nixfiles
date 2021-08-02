{ pkgs, config, ... }:

{
  imports = [
    ./fonts.nix
    ./picom.nix
    ./logind.nix
    ./redshift.nix
  ];

  services.xserver = {
    enable = true;

    # Enable touchpad
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    layout = "us";
    xkbOptions = "caps:swapescape,compose:rctrl";

    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;

          clock-format = "%I:%M %p";

          theme = {
            name = "Materia-dark";
            package = pkgs.materia-theme;
          };

          iconTheme = {
            name = "Papirus-Adapta-Nokto";
            package = pkgs.papirus-icon-theme;
          };
        };

        background = ../../../desktop-backgrounds/earth.png;
      };
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = pkgs.substituteAll {
        src = ./i3/i3config;
        spill_container_script = pkgs.substituteAll {
          src = ./i3/spill_container.sh;
          isExecutable = true;
          inherit (pkgs) runtimeShell jq;
          i3 = config.services.xserver.windowManager.i3.package;
        };
      };

      # Set the desktop background to the current cached lock screen
      # TODO: consider using `feh --no-fehbg --bg-fill --randomize ../../../desktop-backgrounds/*.png`
      # (or find a way to randomize betterlockscreen backgrounds)
      extraSessionCommands = ''
        ${pkgs.betterlockscreen}/bin/betterlockscreen -w
      '';

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
