{ config, lib, ... }:

let
  cfg = config.mixins.vscode;
in
{
  options.mixins.vscode.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the vscode mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.vscode.enable = true;
  };
}
