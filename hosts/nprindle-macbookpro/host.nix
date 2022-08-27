{ self
, nixpkgs
, darwin
, ...
}@inputs:

darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  modules = [
    self.darwinModules.oh-my-zsh

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

