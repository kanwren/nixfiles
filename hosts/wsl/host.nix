{ self
, nixpkgs
, nixos-wsl
, ...
}@inputs:

self.lib.system.makeSystem rec {
  inherit self inputs nixpkgs;

  system = "x86_64-linux";

  modules = [
    nixos-wsl.nixosModules.wsl
    self.nixosModules.base
    ./configuration.nix
  ];
}
