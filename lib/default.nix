{ lib }:

{
  time = import ./time.nix;
  attrsets = import ./attrsets.nix { inherit lib; };
  system = import ./system.nix;
}
