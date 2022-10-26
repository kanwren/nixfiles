{ self }:

{
  imports = [
    (import ./wren.nix { inherit self; })
  ];
}
