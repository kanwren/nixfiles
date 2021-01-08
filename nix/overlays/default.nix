{ neovim }:

{ ... }:

{
  nixpkgs.overlays = [
    # (import ./neovim.nix { inherit neovim; })
    (import ./sudo.nix)
  ];
}
