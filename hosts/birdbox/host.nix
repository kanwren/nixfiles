{ self
, nixpkgs
, home-manager
, nixos-hardware
, sops-nix
, disko
, impermanence
, catppuccin
}:

let
  flakeConfig = import ../../flake.nix;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
    {
      networking.hostName = "birdbox";

      system.stateVersion = "25.05";
      system.configurationRevision = self.rev or self.dirtyRev or null;

      nix = {
        settings = {
          experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
          substituters = flakeConfig.nixConfig.extra-substituters or [ ];
          trusted-public-keys = flakeConfig.nixConfig.extra-trusted-public-keys or [ ];
        };
        channel.enable = false;
      };

      nixpkgs.overlays = [
        self.overlays.default
        self.overlays.fix-open-webui
      ];
    }

    disko.nixosModules.disko

    impermanence.nixosModules.impermanence

    home-manager.nixosModules.home-manager
    {
      home-manager.sharedModules = [
        impermanence.homeManagerModules.impermanence
        catppuccin.homeModules.catppuccin
        self.hmModules.h
        self.hmModules.kubie
        self.hmModules.mixins
      ];
    }

    sops-nix.nixosModules.sops

    (with nixos-hardware.nixosModules; [
      common-pc-laptop
      common-pc-laptop-ssd
      common-pc-laptop-hdd
      common-cpu-intel
      common-gpu-nvidia
    ])

    self.nixosModules.tscaddy

    # configure the bus IDs for common-gpu-nvidia
    {
      hardware.nvidia = {
        prime = {
          offload.enable = true;
          intelBusId = "PCI:0:2:0";
          nvidiaBusId = "PCI:1:0:0";
        };
        open = false;
      };
    }

    ./hardware-configuration.nix

    ./configuration.nix
  ];
}
