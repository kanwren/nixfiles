{ config, lib, ... }:

let
  cfg = config.mixins.kitty;
in
{
  options.mixins.kitty.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the kitty mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      extraConfig = builtins.readFile ./kitty.conf;
      shellIntegration.mode = "enabled";
    };
  };
}

