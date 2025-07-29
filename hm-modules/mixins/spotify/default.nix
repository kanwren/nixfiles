{ pkgs
, config
, lib
, ...
}:
let
  cfg = config.mixins.spotify;
in
{
  options.mixins.spotify.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the spotify mixin";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ spotify ];
  };
}
