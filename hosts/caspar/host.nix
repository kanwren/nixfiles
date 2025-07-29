{ inputs
, outputs
,
}:
outputs.lib.mkDarwinSystem {
  hostname = "caspar";

  system = "aarch64-darwin";

  systemModules = [
    inputs.home-manager.darwinModules.home-manager
    outputs.darwinModules.pueue
    ./configuration.nix
  ];

  overlays = [
    outputs.overlays.stable
    outputs.overlays.additions
  ];

  hmModules = [
    inputs.catppuccin.homeModules.catppuccin
    outputs.hmModules.h
    outputs.hmModules.kubie
    outputs.hmModules.mixins
  ];
}
