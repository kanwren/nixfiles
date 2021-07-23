{ nixpkgs, nix-bundle }:

{
  arx-bundle = import ./arx-bundle.nix { inherit nixpkgs nix-bundle; };
  docker-bundle = import ./docker-bundle.nix { inherit nixpkgs; };
}
