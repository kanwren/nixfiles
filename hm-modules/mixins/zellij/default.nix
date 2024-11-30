{ config, lib, ... }:

let
  cfg = config.mixins.zellij;
in
{
  options.mixins.zellij.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the zellij mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.zellij.enable = true;
  };
}

