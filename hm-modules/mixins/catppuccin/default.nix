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
    catppuccin.flavor = "mocha";

    xdg.enable = true;

    programs.btop.catppuccin.enable = true;
    programs.cava.catppuccin.enable = true;
    programs.kitty.catppuccin.enable = true;
    programs.yazi.catppuccin.enable = true;
    programs.zathura.catppuccin.enable = true;
    programs.zellij.catppuccin.enable = true;
  };
}
