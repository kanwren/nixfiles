{ self
, nixpkgs
, nix-darwin
, ...
}@inputs:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  specialArgs = {
    inherit self;
  };

  modules = [
    inputs.home-manager.darwinModules.home-manager

    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    }

    self.nixosModules.mixins.home-manager-common

    ./configuration.nix
  ];
}

