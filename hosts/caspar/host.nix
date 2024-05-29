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
    self.darwinModules.pueue

    inputs.home-manager.darwinModules.home-manager

    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = nixpkgs.lib.mkForce [ "nixpkgs=${nixpkgs}" ];
    }

    ./configuration.nix
  ];
}

