{ self
, nixpkgs
, nixos-wsl
, ...
}@inputs:

self.lib.system.makeSystem rec {
  inherit nixpkgs;

  system = "x86_64-linux";

  modules = [
    nixos-wsl.nixosModules.wsl
    self.nixosModules.mixins.base
    ./configuration.nix
  ];
}
