{ nlib }:

{ self
, nixpkgs-homepi
, sops-nix
, ...
}@inputs:

let
  nixpkgs = nixpkgs-homepi;
in

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules =
    let
      # extra args to pass to imported modules
      args = {
        inherit inputs;
      };
      # the main configuration
      mainModule = import ./configuration.nix;
      # extra modules from the inputs
      otherModules = [
        sops-nix.nixosModules.sops
        self.nixosModules.duckdns
      ];
      addOverlays = {
        nixpkgs.overlays = [
          self.overlays.raspi-firmware-overlay
        ];
      };
    in
    nixpkgs.lib.flatten [
      nlib.flakes.useFlakes
      (nlib.flakes.pinFlakes { inherit nixpkgs; })

      (nlib.flakes.passArgs args)

      mainModule
      addOverlays
      otherModules
    ];
}

