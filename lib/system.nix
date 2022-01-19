{
  makeSystem = { self, system, inputs, nixpkgs, overlays, modules }: nixpkgs.lib.nixosSystem {
    inherit system;
    modules = modules ++ [
      # pin nixpkgs
      {
        nix.registry.nixpkgs.flake = nixpkgs;
        nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
      }

      # inject custom args
      {
        config._module.args = {
          inherit system self inputs;
        };
      }

      {
        nixpkgs.overlays = overlays;
      }
    ];
  };
}
