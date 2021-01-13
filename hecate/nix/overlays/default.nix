deps:

{ ... }:

{
  nixpkgs.overlays = [
    (import ./neovim.nix deps)
    (import ./sudo.nix)
  ];
}
