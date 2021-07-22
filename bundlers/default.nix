{ nixpkgs }:

{
  arx-bundle = import ./arx-bundle.nix { inherit nixpkgs; };
  docker-bundle = import ./docker-bundle.nix { inherit nixpkgs; };
}
