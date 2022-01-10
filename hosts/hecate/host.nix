{ self
, nixpkgs
, nixos-hardware
, nur
, home-manager
, ...
}@inputs:

let
  mergeAttrs = builtins.foldl' (x: y: x // y) { };
in

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
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
        custom = {
          pkgs = mergeAttrs [
            self.legacyPackages.${system}
            self.packages.${system}
          ];
          inherit (self) hmModules;
          inherit (self) lib;
        };
      };
    }

    # hardware modules
    (with nixos-hardware.nixosModules; [
      common-pc-laptop
      common-pc-laptop-ssd
      common-cpu-amd
      common-gpu-nvidia
      # configure the bus IDs for common-gpu-nvidia
      {
        hardware.nvidia.prime = {
          intelBusId = "PCI:5:0:0";
          nvidiaBusId = "PCI:1:0:0";
        };
      }
      ./hardware-configuration.nix
    ])

    {
      nixpkgs.overlays = [
        nur.overlay
        self.overlays.fix-h-warning
      ];
    }

    home-manager.nixosModules.home-manager

    ./configuration.nix
  ];
}
