{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.nix;
in {
  options.mixins.nix.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the nix mixin";
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."nixpkgs/config.nix".source = ./config.nix;

    home.packages = with pkgs; [
      comma
      nix-tree
      nix-diff
    ];
  };
}
