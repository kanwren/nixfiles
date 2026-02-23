{
  config,
  lib,
  ...
}:
let
  cfg = config.mixins.yazi;
in
{
  options.mixins.yazi = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.enable;
      description = "Whether to enable the yazi mixin";
    };
    shellIntegration.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable yazi shell integration";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      shellWrapperName = "y";
      enableBashIntegration = cfg.shellIntegration.enable;
      enableZshIntegration = cfg.shellIntegration.enable;
      enableFishIntegration = cfg.shellIntegration.enable;
    };
  };
}
