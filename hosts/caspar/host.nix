{ self
, nixpkgs
, nix-darwin
, home-manager
, catppuccin
}:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  modules = [
    {
      system.configurationRevision = self.rev or self.dirtyRev or null;
    }

    {
      nix.registry.nixpkgs.to = { type = "path"; path = nixpkgs.outPath; };
      nix.nixPath = nixpkgs.lib.mkForce [ "nixpkgs=flake:nixpkgs" ];
      nixpkgs.overlays = [ self.overlays.default ];
    }

    self.darwinModules.pueue

    home-manager.darwinModules.home-manager
    {
      home-manager.sharedModules = [
        catppuccin.homeModules.catppuccin
        self.hmModules.h
        self.hmModules.kubie
        self.hmModules.mixins
      ];
    }

    ./configuration.nix
  ];
}

