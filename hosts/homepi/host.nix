{ self
, nixpkgs-homepi
, sops-nix
, ...
}@inputs:

let
  nixpkgs = nixpkgs-homepi;
in

self.lib.system.makeSystem {
  inherit self inputs nixpkgs;

  system = "aarch64-linux";

  overlays = [
    self.overlays.raspi-firmware-overlay
  ];

  modules = nixpkgs.lib.flatten [
    sops-nix.nixosModules.sops
    self.nixosModules.duckdns
    ./configuration.nix
  ];
}

