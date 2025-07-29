{ config
, lib
, ...
}:
let
  cfg = config.mixins.cava;
in
{
  options.mixins.cava.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the cava mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.cava.enable = true;
  };
}
