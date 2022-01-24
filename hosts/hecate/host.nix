{ self
, nixpkgs
, nixos-hardware
, home-manager
, ...
}@inputs:

self.lib.system.makeSystem rec {
  inherit self inputs nixpkgs;

  system = "x86_64-linux";

  overlays = [
    self.overlays.fix-h-warning
  ];

  modules = nixpkgs.lib.flatten [
    home-manager.nixosModules.home-manager

    # hardware modules
    (with nixos-hardware.nixosModules; [
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
      base
      home-manager-base
      tailscale

      desktop.base
      desktop.x.base
      desktop.x.i3
      desktop.audio
      desktop.bluetooth
      desktop.virtualisation

      users.nprin.base
      users.nprin.home
    ])

    ./configuration.nix
  ];
}
