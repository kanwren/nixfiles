{ self
, nixpkgs
, ...
}@inputs:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
      nixpkgs.overlays = [ ];
    }

    inputs.home-manager.nixosModules.home-manager

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

    (with self.nixosModules.mixins; [
      base.full
      home-manager-common

      desktop.base
      desktop.x.i3
      desktop.audio
      desktop.bluetooth
      desktop.virtualisation

      tailscale
    ])

    ./hardware-configuration.nix
    (import ./configuration.nix { inherit self; })
  ];
}
