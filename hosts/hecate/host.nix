{ inputs
, outputs
,
}:
outputs.lib.mkNixosSystem {
  hostname = "hecate";

  systemModules = [
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    outputs.nixosModules.tscaddy
    outputs.nixosModules.pueue

    ./hardware-configuration.nix
    ./configuration.nix
  ];

  overlays = [
    outputs.overlays.stable
    outputs.overlays.fixes
    outputs.overlays.additions
  ];

  hmModules = [
    inputs.catppuccin.homeModules.catppuccin
    outputs.hmModules.h
    outputs.hmModules.kubie
    outputs.hmModules.mixins
  ];
}
