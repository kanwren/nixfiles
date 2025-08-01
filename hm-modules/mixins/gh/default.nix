{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.gh;
in {
  options.mixins.gh.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the gh mixin";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.gitAndTools.gh
    ];
  };
}
