{ self
, nixpkgs-homepi
, sops-nix
, ...
}@inputs:

let
  nixpkgs = nixpkgs-homepi;
in

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";

  overlays = [
    self.overlays.raspi-firmware-overlay
  ];

  modules = nixpkgs.lib.flatten [
    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    }

    sops-nix.nixosModules.sops

    self.nixosModules.duckdns
    self.nixosModules.mixins.tailscale

    ./configuration.nix
  ];
}

