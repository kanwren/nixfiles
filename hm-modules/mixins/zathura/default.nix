{ config, lib, ... }:

let
  cfg = config.mixins.zathura;
in
{
  options.mixins.zathura.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the zathura mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = {
        statusbar-h-padding = 0;
        statusbar-v-padding = 0;
        page-padding = 1;
      };
      extraConfig = ''
        map i recolor
        map p print
      '';
    };
  };
}

