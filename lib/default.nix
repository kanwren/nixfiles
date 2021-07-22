{ lib }:

{
  types = import ./types.nix { inherit lib; };
  time = import ./time.nix;
  attrsets = import ./attrsets.nix { inherit lib; };
  flakes = import ./flakes.nix { };
}
