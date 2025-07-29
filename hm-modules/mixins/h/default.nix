# Requires h home-manager module
{ config
, lib
, ...
}:
let
  cfg = config.mixins.h;
in
{
  options.mixins.h.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the h mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.h.enable = true;
  };
}
