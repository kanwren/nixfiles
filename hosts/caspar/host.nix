{ self
, nixpkgs
, darwin
, ...
}@inputs:

darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  specialArgs = {
    inherit self;
  };

  modules = [
    inputs.home-manager.darwinModules.home-manager
    self.darwinModules.oh-my-zsh
    self.darwinModules.starship

    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    }

    self.nixosModules.mixins.home-manager-common
    self.nixosModules.mixins.base.starship

    ./configuration.nix
  ];
}

