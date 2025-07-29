{ config
, lib
, ...
}:
let
  cfg = config.mixins.direnv;
in
{
  options.mixins.direnv.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the direnv mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
