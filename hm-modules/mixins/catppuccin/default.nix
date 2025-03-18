# Requires catppuccin home-manager module

{ config, lib, ... }:

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
    xdg.enable = true;

    catppuccin = {
      flavor = "mocha";

      btop.enable = true;
      cava.enable = true;
      k9s.enable = true;
      kitty.enable = true;
      yazi.enable = true;
      zathura.enable = true;
      zellij.enable = true;
    };
  };
}
