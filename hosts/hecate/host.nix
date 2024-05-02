{ self
, nixpkgs
, ...
}@inputs:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
    # Pass this flake recurisvely as a module input 'flake'
    { _module.args.flake = self; }

    # Bootstrapping
    ({ config, ... }: {
      networking.hostName = "hecate";

      system.stateVersion = "22.11";

      # Set up flakes, inject nixpkgs, and remove impure components
      nix = {
        # Enable flakes
        settings.experimental-features = [ "nix-command" "flakes" ];

        # The global flake registry's nixpkgs should be the system nixpkgs
        registry.nixpkgs.flake = nixpkgs;

        # '<nixpkgs>' should be the system nixpkgs
        nixPath = [ "nixpkgs=${nixpkgs}" ];
        settings.nix-path = config.nix.nixPath; # workaround for https://github.com/NixOS/nix/issues/9574; NIX_PATH doesn't work when channel.enable = false sets `nix-path = ""`

        # Disable channels
        channel.enable = false;
      };
    })

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
