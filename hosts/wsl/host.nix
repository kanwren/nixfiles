{ nlib }:

{ self
, nixpkgs
, nixos-wsl
, ...
}@inputs:

nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  modules = [
    self.nixosModules.mixins.use-flakes
    {
      nix.registry.nixpkgs.flake = nixpkgs;
      nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
      config._module.args = {
        inherit inputs;
      };
    }
    nixos-wsl.nixosModule
    ./configuration.nix
  ];
}
