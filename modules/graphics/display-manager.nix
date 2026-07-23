{ config, ... }:

{
  flake.modules.nixos.graphics = {
    imports = [ config.flake.modules.nixos.noctalia ];
  };
}
