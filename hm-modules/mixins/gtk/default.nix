{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.gtk;
in {
  options.mixins.gtk.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the gtk mixin";
  };

  config = lib.mkIf cfg.enable {
    gtk = {
      enable = true;
      font = {
        name = "FiraCode Nerd Font Mono";
      };
      theme = {
        name = "catppuccin-mocha-lavender-standard";
        package = pkgs.catppuccin-gtk.override {
          accents = ["lavender"];
          variant = "mocha";
        };
      };
      iconTheme = {
        name = "Papirus";
        package = pkgs.papirus-icon-theme;
      };
    };

    qt = {
      enable = true;
      platformTheme.name = "gtk";
    };
  };
}
