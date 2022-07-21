{
  makeSystem = { nixpkgs, system, overlays ? [ ], modules ? [ ] }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      modules = modules ++ [
        # pin nixpkgs
        {
          nix.registry.nixpkgs.flake = nixpkgs;
          nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
        }

        {
          nixpkgs.overlays = overlays;
        }
      ];
    };
}
