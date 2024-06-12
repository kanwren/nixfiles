{ config, lib, ... }:

let
  cfg = config.mixins.firefox;
in
{
  options.mixins.firefox.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the firefox mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      # TODO: user chrome
    };
  };
}
