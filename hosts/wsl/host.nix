{ self
, nixpkgs
, nixos-wsl
, ...
}@inputs:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";

  modules = [
    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    }

    nixos-wsl.nixosModules.wsl
    self.nixosModules.mixins.base
    ./configuration.nix
  ];
}
