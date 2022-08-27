{ self
, nixpkgs
, darwin
, ...
}@inputs:

darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  modules = [
    self.darwinModules.oh-my-zsh
    self.darwinModules.starship
    self.nixosModules.mixins.base.starship

    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    }

    {
      config._module.args = {
        inherit self;
      };
    }

    ./configuration.nix
  ];
}

