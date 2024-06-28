{ self
, nixpkgs
, lix-module
, nix-darwin
, home-manager
, catppuccin
}:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  modules = [
    {
      nix.registry.nixpkgs.to = { type = "path"; path = nixpkgs.outPath; };
      nix.nixPath = nixpkgs.lib.mkForce [ "nixpkgs=flake:nixpkgs" ];
      nixpkgs.overlays = [ self.overlays.default ];
    }

    lix-module.nixosModules.default

    self.darwinModules.pueue

    home-manager.darwinModules.home-manager
    {
      home-manager.sharedModules = [
        catppuccin.homeManagerModules.catppuccin
        self.hmModules.h
        self.hmModules.mixins
      ];
    }

    ./configuration.nix
  ];
}

