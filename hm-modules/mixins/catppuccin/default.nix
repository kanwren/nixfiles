# Requires catppuccin home-manager module
{
  config,
  lib,
  ...
}:
let
  cfg = config.mixins.catppuccin;
in
{
  options.mixins.catppuccin.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the catppuccin mixin";
  };

  config = lib.mkIf cfg.enable {
    catppuccin = {
      enable = true;
      flavor = "mocha";
      accent = "lavender";
      kvantum.enable = false;
      rofi.enable = false;
      firefox.enable = false;
      yazi.enable = false;
    };

    xdg.enable = true;
  };
}
