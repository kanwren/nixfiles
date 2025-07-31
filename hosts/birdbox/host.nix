{
  inputs,
  outputs,
}:
outputs.lib.mkNixosSystem {
  hostname = "birdbox";

  systemModules = [
    inputs.disko.nixosModules.disko
    inputs.impermanence.nixosModules.impermanence
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.common-pc-laptop-hdd
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    outputs.nixosModules.tscaddy
    outputs.nixosModules.gitit

    ./hardware-configuration.nix
    ./configuration.nix
  ];

  overlays = [
    outputs.overlays.stable
    outputs.overlays.fixes
    outputs.overlays.additions
  ];

  hmModules = [
    inputs.impermanence.homeManagerModules.impermanence
    inputs.catppuccin.homeModules.catppuccin
    outputs.hmModules.h
    outputs.hmModules.kubie
    outputs.hmModules.mixins
  ];
}
