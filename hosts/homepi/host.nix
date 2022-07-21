{ self
, nixpkgs-homepi
, sops-nix
, ...
}@inputs:

let
  nixpkgs = nixpkgs-homepi;
in

self.lib.system.makeSystem {
  inherit nixpkgs;

  system = "aarch64-linux";

  overlays = [
    self.overlays.raspi-firmware-overlay
  ];

  modules = nixpkgs.lib.flatten [
    sops-nix.nixosModules.sops

    self.nixosModules.duckdns
    self.nixosModules.mixins.tailscale

    ./configuration.nix
  ];
}

