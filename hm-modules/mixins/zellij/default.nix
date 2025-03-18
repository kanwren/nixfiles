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
    enableShellIntegration = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to enable zellij shell integration";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zellij = {
      enable = true;
    };

    home.sessionVariables = {
      ZELLIJ_AUTO_ATTACH = if cfg.enableShellIntegration then "true" else "false";
      ZELLIJ_AUTO_EXIT = if cfg.enableShellIntegration then "true" else "false";
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

