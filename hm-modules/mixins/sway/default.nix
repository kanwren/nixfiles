{
  pkgs,
  lib,
  config,
  ...
}:

# TODO:
# - test marking bindings
# - add cliphist
# - test swaylock, swaylock aesthetics

let
  cmds = {
    waybar = lib.getExe pkgs.waybar;
    swaylock = lib.getExe pkgs.swaylock;
    qalculate-gtk = lib.getExe pkgs.qalculate-gtk;
    playerctl = lib.getExe pkgs.playerctl;
    brightnessctl = lib.getExe pkgs.brightnessctl;
    amixer = lib.getExe' pkgs.alsa-utils "amixer";
    shotman = lib.getExe' pkgs.shotman "shotman";
    dmenu = lib.getExe pkgs.dmenu;
    rofi = lib.getExe pkgs.rofi;
    kitty = lib.getExe pkgs.kitty;
  };

  colors = {
    background = "#131020";
    base = "#1e1e2e";
    text = "#cdd6f4";
    pink = "#f5c2e7";
    lavender = "#b4befe";
    blue = "#89b4fa";
    red = "#f38ba8";
  };
in

{
  options.mixins.sway.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the sway mixin";
  };

  config = lib.mkIf config.mixins.sway.enable {
    home.packages = [
      pkgs.alsa-utils
      pkgs.brightnessctl
      pkgs.playerctl
      pkgs.shotman
      pkgs.swaylock
      pkgs.wayclip
    ];

    wayland.windowManager.sway = {
      enable = true;
      xwayland = false;
      config =
        let
          cfg = config.wayland.windowManager.sway.config;
        in
        {
          modifier = "Mod4";
          workspaceLayout = "default";
          gaps = {
            inner = 10;
            outer = 10;
          };
          terminal = cmds.kitty;
          menu = "${cmds.rofi} -modi drun,run,ssh -show drun -fuzzy";
          fonts = {
            names = [ "DejaVu Sans Mono" ];
            size = 12.0;
          };
          floating = {
            inherit (cfg) modifier;
            titlebar = false;
            border = 1;
            criteria = [
              { class = "^Thunar$"; }
              { class = "^Qalculate-gtk$"; }
            ];
          };
          window = {
            titlebar = false;
            border = 1;
            hideEdgeBorders = "both";
            commands = [
              {
                criteria.class = "^discord$";
                command = "mark d";
              }
              {
                criteria.class = "^Spotify$";
                command = "mark s";
              }
              {
                criteria.class = "^Bitwarden$";
                command = "mark p";
              }
            ];
          };
          focus = {
            newWindow = "smart";
            followMouse = "yes";
            wrapping = "yes";
            mouseWarping = false;
          };
          keybindings = {
            "${cfg.modifier}+i" = "exec echo \"\" | ${cmds.dmenu} -p cmd | xargs -r swaymsg"; # TODO: goto mark
            "${cfg.modifier}+Return" = "exec ${cfg.terminal}";
            "${cfg.modifier}+Shift+q" = "kill";
            "${cfg.modifier}+d" = "exec ${cfg.menu}";
            "${cfg.modifier}+${cfg.left}" = "focus left";
            "${cfg.modifier}+${cfg.down}" = "focus down";
            "${cfg.modifier}+${cfg.up}" = "focus up";
            "${cfg.modifier}+${cfg.right}" = "focus right";
            "${cfg.modifier}+Shift+${cfg.left}" = "move left";
            "${cfg.modifier}+Shift+${cfg.down}" = "move down";
            "${cfg.modifier}+Shift+${cfg.up}" = "move up";
            "${cfg.modifier}+Shift+${cfg.right}" = "move right";
            "${cfg.modifier}+b" = "splith";
            "${cfg.modifier}+v" = "splitv";
            "${cfg.modifier}+f" = "fullscreen toggle";
            "${cfg.modifier}+a" = "focus parent";
            "${cfg.modifier}+Shift+a" = "focus child";
            "${cfg.modifier}+s" = "layout stacking";
            "${cfg.modifier}+w" = "layout tabbed";
            "${cfg.modifier}+e" = "layout toggle split";
            "${cfg.modifier}+Space" = "focus mode_toggle";
            "${cfg.modifier}+Shift+Space" = "floating toggle";
            "${cfg.modifier}+1" = "workspace number 1";
            "${cfg.modifier}+2" = "workspace number 2";
            "${cfg.modifier}+3" = "workspace number 3";
            "${cfg.modifier}+4" = "workspace number 4";
            "${cfg.modifier}+5" = "workspace number 5";
            "${cfg.modifier}+6" = "workspace number 6";
            "${cfg.modifier}+7" = "workspace number 7";
            "${cfg.modifier}+8" = "workspace number 8";
            "${cfg.modifier}+9" = "workspace number 9";
            "${cfg.modifier}+0" = "workspace number 10";
            "${cfg.modifier}+Shift+1" = "move container to workspace number 1";
            "${cfg.modifier}+Shift+2" = "move container to workspace number 2";
            "${cfg.modifier}+Shift+3" = "move container to workspace number 3";
            "${cfg.modifier}+Shift+4" = "move container to workspace number 4";
            "${cfg.modifier}+Shift+5" = "move container to workspace number 5";
            "${cfg.modifier}+Shift+6" = "move container to workspace number 6";
            "${cfg.modifier}+Shift+7" = "move container to workspace number 7";
            "${cfg.modifier}+Shift+8" = "move container to workspace number 8";
            "${cfg.modifier}+Shift+9" = "move container to workspace number 9";
            "${cfg.modifier}+Shift+0" = "move container to workspace number 10";
            "${cfg.modifier}+Left" = "workspace prev";
            "${cfg.modifier}+Right" = "workspace next";
            "${cfg.modifier}+Tab" = "workspace back_and_forth";
            "${cfg.modifier}+minus" = "scratchpad show";
            "${cfg.modifier}+Shift+minus" = "move scratchpad";
            "${cfg.modifier}+c" = "mode control";
            "${cfg.modifier}+r" = "mode resize";
            "${cfg.modifier}+z" = "mode barcontrol";
            "${cfg.modifier}+m" = "exec echo \"\" | ${cmds.dmenu} -p mark | xargs -r swaymsg mark --add"; # TODO: mark
            "${cfg.modifier}+Shift+m" = "exec echo \"\" | ${cmds.dmenu} -p unmark | xargs -r swaymsg unmark"; # TODO: unmark
            "${cfg.modifier}+Control+Shift+m" = "unmark";
            "${cfg.modifier}+g" =
              "exec echo \"\" | ${cmds.dmenu} -p goto | xargs -r -I__ swaymsg \"[con_mark=__] focus\"";
            "${cfg.modifier}+bracketleft" = "mark --add swapee";
            "${cfg.modifier}+bracketright" = "swap container with mark swapee; unmark swapee";
            "Print" = "exec ${cmds.shotman} -c output";
            "Print+Shift" = "exec ${cmds.shotman} -c region";
            "Print+Control+Shift" = "exec ${cmds.shotman} -c window";
            "XF86AudioRaiseVolume" = "exec --no-startup-id ${cmds.amixer} sset Master 5%+"; # Increase sound volume
            "XF86AudioLowerVolume" = "exec --no-startup-id ${cmds.amixer} sset Master 5%-"; # Decrease sound volume
            "XF86AudioMute" = "exec --no-startup-id ${cmds.amixer} sset Master 1+ toggle"; # Toggle mute
            "XF86MonBrightnessDown" = "exec ${cmds.brightnessctl} set 5%-";
            "XF86MonBrightnessUp" = "exec ${cmds.brightnessctl} set +5%";
            "XF86AudioPlay" = "exec ${cmds.playerctl} play-pause";
            "XF86AudioPause" = "exec ${cmds.playerctl} pause";
            "XF86AudioNext" = "exec ${cmds.playerctl} next";
            "XF86AudioPrev" = "exec ${cmds.playerctl} previous";
            "XF86Calculator" = "exec ${cmds.qalculate-gtk}";
            "XF86PowerOff" = "mode system";
          };
          modes = {
            resize = {
              "${cfg.left}" = "resize shrink width 10 px";
              "${cfg.down}" = "resize grow height 10 px";
              "${cfg.up}" = "resize shrink height 10 px";
              "${cfg.right}" = "resize grow width 10 px";
              "Escape" = "mode default";
              "Return" = "mode default";
            };
            control = {
              "l" = "exec ${cmds.swaylock}";
              "r" = "restart";
              "Shift+e" = "swaymsg exit";
              "Shift+s" = "shutdown now";
              "Shift+r" = "shutdown -r now";
              "Escape" = "mode default";
              "Return" = "mode default";
            };
            barcontrol = {
              "${cfg.modifier}+d" = "bar mode dock";
              "${cfg.modifier}+h" = "bar mode hide";
              "${cfg.modifier}+i" = "bar mode invisible";
              "Escape" = "mode default";
              "Return" = "mode default";
            };
          };
          input = {
            "type:keyboard" = {
              "xkb_layout" = "us";
              "xkb_options" = "caps:escape";
            };
          };
          output = {
            "*" = {
              "background" = "${../../../desktop-backgrounds/dark-cat.png} fill";
            };
          };
          bars = [
            {
              mode = "dock";
              hiddenState = "hide";
              position = "bottom";
              workspaceButtons = true;
              workspaceNumbers = true;
              statusCommand = cmds.waybar;
              fonts = {
                names = [ "monospace" ];
                size = 8.0;
              };
              trayOutput = "primary";
              colors = {
                inherit (colors) background;
                statusline = colors.text;
                focusedStatusline = colors.text;
                separator = colors.text;
                focusedSeparator = colors.text;
                focusedWorkspace = {
                  border = colors.base;
                  background = colors.pink;
                  text = colors.base;
                };
                activeWorkspace = {
                  border = colors.base;
                  background = colors.base;
                  text = colors.blue;
                };
                inactiveWorkspace = {
                  border = colors.base;
                  background = colors.base;
                  inherit (colors) text;
                };
                urgentWorkspace = {
                  border = colors.base;
                  background = colors.base;
                  text = colors.red;
                };
                bindingMode = {
                  border = colors.base;
                  background = colors.lavender;
                  text = colors.base;
                };
              };
            }
          ];
        };
    };

    services = {
      wlsunset = {
        enable = true;
        latitude = 34.052235;
        longitude = -118.243683;
        temperature = {
          day = 6500;
          night = 3200;
        };
        gamma = 0.8;
      };
    };
  };
}
