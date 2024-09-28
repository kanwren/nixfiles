{ self
, nixpkgs
, lix-module
, home-manager
, nixos-hardware
, catppuccin
}:

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
    # Bootstrapping
    ({ config, ... }: {
      networking.hostName = "hecate";

      system.stateVersion = "22.11";

      # Set up flakes and remove impure components. `nixosSystem` already
      # injects `nixpkgs` into system flake registry and `NIX_PATH`.
      nix = {
        settings.experimental-features = [ "nix-command" "flakes" ];
        channel.enable = false;
        settings.nix-path = config.nix.nixPath; # workaround for https://github.com/NixOS/nix/issues/9574; NIX_PATH doesn't work when channel.enable = false sets `nix-path = ""`
      };

      nixpkgs.overlays = [ self.overlays.default ];
    })

    lix-module.nixosModules.default

    home-manager.nixosModules.home-manager
    {
      home-manager.sharedModules = [
        catppuccin.homeManagerModules.catppuccin
        self.hmModules.h
        self.hmModules.mixins
      ];
    }

    self.nixosModules.pueue

    # hardware modules
    (with nixos-hardware.nixosModules; [
      common-pc-laptop
      common-pc-laptop-ssd
      common-cpu-amd
      common-gpu-nvidia
      {
        # configure the bus IDs for common-gpu-nvidia
        hardware.nvidia = {
          prime = {
            intelBusId = "PCI:5:0:0";
            nvidiaBusId = "PCI:1:0:0";
          };
          open = false;
        };
      }
    ])

    ./hardware-configuration.nix
    ./configuration.nix
  ];
}
