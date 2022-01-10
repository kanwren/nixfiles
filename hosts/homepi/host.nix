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
  modules = nixpkgs.lib.flatten [
    self.nixosModules.mixins.use-flakes
    # pin nixpkgs
    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    }

    {
      config._module.args = {
        inherit inputs;
      };
    }

    {
      nixpkgs.overlays = [
        self.overlays.raspi-firmware-overlay
      ];
    }

    sops-nix.nixosModules.sops
    self.nixosModules.duckdns

    ./configuration.nix
  ];
}

