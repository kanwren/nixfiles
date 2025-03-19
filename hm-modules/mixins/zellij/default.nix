{ config, lib, ... }:

let
  cfg = config.mixins.zellij;
in
{
  options.mixins.zellij = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.enable;
      description = "Whether to enable the zellij mixin";
    };
    shellIntegration.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to enable zellij shell integration";
    };
    shellIntegration.autoAttach = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether zellij should auto-attach/auto-exit";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zellij = {
      enable = true;
      enableBashIntegration = cfg.shellIntegration.enable;
      enableZshIntegration = cfg.shellIntegration.enable;
      enableFishIntegration = cfg.shellIntegration.enable;
    };

    home.sessionVariables =
      let
        autoAttach = cfg.shellIntegration.enable && cfg.shellIntegration.autoAttach;
        autoAttachStr = if autoAttach then "true" else "false";
      in
      {
        ZELLIJ_AUTO_ATTACH = autoAttachStr;
        ZELLIJ_AUTO_EXIT = autoAttachStr;
      };

    # NOTE: the home-manager toKDL generator represents node children as an
    # attrset, which is the wrong type for KDL's model (can't repeat keys, and
    # children are supposed to be ordered). This makes it impossible to use
    # toKDL to generate repeated children with the same node name, as is needed
    # here for keybindings.
    xdg.configFile."zellij/config.kdl" = lib.mkForce {
      source = ./config.kdl;
    };
  };
}

