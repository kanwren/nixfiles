{ self
, nixpkgs
, ...
}@inputs:

self.lib.system.makeSystem rec {
  inherit self inputs nixpkgs;

  system = "x86_64-linux";

  modules = nixpkgs.lib.flatten [
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
      base
      home-manager

      desktop.base
      desktop.x.base
      desktop.x.i3
      desktop.audio
      desktop.bluetooth
      desktop.virtualisation

      users.wren.base
      users.wren.home

      tailscale
    ])

    ./hardware-configuration.nix
    ./configuration.nix
  ];
}
