{ config, lib, ... }:

let
  cfg = config.mixins.jq;
in
{
  options.mixins.jq.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the jq mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.jq.enable = true;
  };
}
