{ self
, nixpkgs
, nix-darwin
, home-manager
, catppuccin
}:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  modules = [
    self.darwinModules.pueue

    home-manager.darwinModules.home-manager
    {
      home-manager.sharedModules = [
        catppuccin.homeManagerModules.catppuccin
        self.hmModules.h
        self.hmModules.mixins
      ];
    }

    {
      nix.registry.nixpkgs.to = { type = "path"; path = nixpkgs.outPath; };
      nix.nixPath = nixpkgs.lib.mkForce [ "nixpkgs=flake:nixpkgs" ];
      nixpkgs.overlays = [ self.overlays.default ];
    }

    ./configuration.nix
  ];
}

