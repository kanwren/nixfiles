{ pkgs
, config
, ...
}: {
  imports = [
    ./picom.nix
    ./redshift.nix
  ];

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchExternalPower = "ignore";
    lidSwitchDocked = "ignore";
    extraConfig = ''
      IdleAction=ignore
      HandlePowerKey=ignore
    '';
  };

  services.xserver = {
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3;

      configFile = pkgs.replaceVars ./i3config/i3config {
        spill_container_script = pkgs.replaceVarsWith {
          src = ./i3config/spill_container.bash;
          isExecutable = true;
          replacements = {
            inherit (pkgs) runtimeShell jq;
            i3 = config.services.xserver.windowManager.i3.package;
          };
        };

        pick_game_script = pkgs.replaceVarsWith {
          src = ./i3config/pick_game.bash;
          isExecutable = true;
          replacements = {
            inherit (pkgs) runtimeShell rofi;
          };
        };

        copyq_launch_script = pkgs.replaceVarsWith {
          src = ./i3config/copyq_launch.bash;
          isExecutable = true;
          replacements = {
            inherit (pkgs) runtimeShell copyq;
          };
        };

        i3status_config = pkgs.writeText "i3status.conf" ''
          general {
                  colors = true
                  color_good = "#a6e3a1"
                  color_degraded = "#f9e2af"
                  color_bad = "#f38ba8"
                  interval = 5
          }

          order += "wireless _first_"
          order += "ethernet _first_"
          order += "battery all"
          order += "memory"
          order += "disk /"
          order += "tztime local"

          wireless _first_ {
                  format_up = "W %quality at %essid"
                  format_down = "W down"
          }

          ethernet _first_ {
                  format_up = "E %speed"
                  format_down = "E down"
          }

          battery all {
                  format = "%status %percentage %remaining"
          }

          memory {
                  format = "%used/%available"
                  threshold_degraded = "1G"
                  format_degraded = "MEM < %available"
          }

          disk "/" {
                  format = "%avail"
          }

          tztime local {
                  format = "%Y-%m-%d %H:%M:%S"
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
        # Media controls
        playerctl
        alsa-utils
      ];
    };
  };

  programs = {
    nm-applet.enable = true;
  };
}
