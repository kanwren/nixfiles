{
  config,
  lib,
  ...
}:
let
  cfg = config.mixins.yazi;
in
{
  options.mixins.yazi.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the yazi mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.yazi = {
      enable = true;

      enableBashIntegration = true;
      enableFishIntegration = true;
      shellWrapperName = "y";

      settings = {
        manager = {
          show_hidden = true;
          sort_by = "natural";
          sort_sensitive = true;
          sort_dir_first = true;
          show_symlink = true;
        };
      };
    };
  };
}
