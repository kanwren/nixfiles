{ lib }:

{
  attrsets = import ./attrsets.nix { inherit lib; };
  system = import ./system.nix;
}
