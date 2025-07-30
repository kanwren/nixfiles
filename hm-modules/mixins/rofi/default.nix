{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.rofi;
in {
  options.mixins.rofi.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the rofi mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      terminal = "${pkgs.kitty}/bin/kitty";
      font = "FiraCode Nerd Font 11";
      extraConfig = {
        show-icons = true;
        icon-theme = "hicolor";
        drun-display-format = "{name}";
        disable-history = false;
        fullscreen = false;
        hide-scrollbar = true;
        sidebar-mode = false;

        display-ssh = " [ssh]";
        display-run = " [run]";
        display-drun = "󱓞 [app]";
        display-window = " [window]";
        display-combi = " [combi]";
      };
    };
  };
}
