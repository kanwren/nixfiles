{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.rust;
in {
  options.mixins.rust.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the rust mixin";
  };

  config = lib.mkIf cfg.enable {
    home.sessionPath = [
      "$HOME/.cargo/bin"
    ];

    home.packages = with pkgs; [
      rustup

      # cargo extensions
      cargo-cache
      # cargo-download
      cargo-feature
      cargo-watch
      cargo-wipe
    ];
  };
}
