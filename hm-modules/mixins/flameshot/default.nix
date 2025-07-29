{ config
, lib
, ...
}:
let
  cfg = config.mixins.flameshot;
in
{
  options.mixins.flameshot.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the flameshot mixin";
  };

  config = lib.mkIf cfg.enable {
    services.flameshot = {
      enable = true;
    };
  };
}
