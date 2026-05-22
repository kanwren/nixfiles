{ lib, ... }:

{
  options.flake.meta = lib.mkOption {
    type = lib.types.lazyAttrsOf lib.types.anything;
  };

  config.flake.meta.name = "kanwren/nixfiles";
}
