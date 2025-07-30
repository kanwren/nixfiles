{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.browsers;
in {
  options.mixins.browsers = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.enable;
      description = "Whether to enable the browsers mixin";
    };

    firefox.enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.browsers.enable;
      description = "Whether to enable the firefox mixin";
    };

    chromium.enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.browsers.enable;
      description = "Whether to enable the chromium mixin";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.firefox = lib.mkIf cfg.firefox.enable {
      enable = true;
      # TODO: user chrome
    };

    programs.chromium = lib.mkIf cfg.chromium.enable {
      enable = true;
      package = pkgs.ungoogled-chromium;
    };
  };
}
