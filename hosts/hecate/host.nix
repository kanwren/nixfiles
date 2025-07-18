{ self
, nixpkgs
, home-manager
, nixos-hardware
, sops-nix
, catppuccin
}:

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
    # Bootstrapping
    ({ config, ... }: {
      networking.hostName = "hecate";

      system.stateVersion = "25.05";
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Set up flakes and remove impure components. `nixosSystem` already
      # injects `nixpkgs` into system flake registry and `NIX_PATH`.
      nix = {
        settings.experimental-features = [ "nix-command" "flakes" ];
        channel.enable = false;
      };

      nixpkgs.overlays = [
        self.overlays.default
        # https://github.com/NixOS/nixpkgs/issues/424692
        self.overlays.fix-open-webui
      ];
    })

    home-manager.nixosModules.home-manager
    {
      home-manager.sharedModules = [
        catppuccin.homeModules.catppuccin
        self.hmModules.h
        self.hmModules.kubie
        self.hmModules.mixins
      ];
    }

    sops-nix.nixosModules.sops
    {
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    }

    self.nixosModules.pueue

    # hardware modules
    (with nixos-hardware.nixosModules; [
      common-pc-laptop
      common-pc-laptop-ssd
      common-cpu-amd
      common-gpu-nvidia
    ])

    # configure the bus IDs for common-gpu-nvidia
    {
      hardware.nvidia = {
        prime = { intelBusId = "PCI:5:0:0"; nvidiaBusId = "PCI:1:0:0"; };
        open = false;
      };
    }

    ./hardware-configuration.nix
    ./configuration.nix
  ];
}
