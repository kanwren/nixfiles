{ self
, nixpkgs
, ...
}@inputs:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
    # Pass this flake recurisvely as a module input 'flake'
    {
      _module.args.flake = self;
    }

    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
      nixpkgs.overlays = [ ];
    }

    inputs.home-manager.nixosModules.home-manager

    self.nixosModules.pueue

    # hardware modules
    (with inputs.nixos-hardware.nixosModules; [
      common-pc-laptop
      common-pc-laptop-ssd
      common-cpu-amd
      common-gpu-nvidia
      {
        # configure the bus IDs for common-gpu-nvidia
        hardware.nvidia.prime = {
          intelBusId = "PCI:5:0:0";
          nvidiaBusId = "PCI:1:0:0";
        };
      }
    ])

    ./hardware-configuration.nix
    ./configuration.nix
  ];
}
