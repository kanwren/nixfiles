{
  config,
  lib,
  ...
}:
let
  cfg = config.mixins.zoxide;
in
{
  options.mixins.zoxide.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the zoxide mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.zoxide.enable = true;
  };
}
