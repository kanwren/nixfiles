{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    make-shell.url = "github:nicknovitski/make-shell";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    inputs:
    let
      inherit (inputs.nixpkgs) lib;
      isFlakeModule = f: f.type == "regular" && f.hasExt "nix" && !lib.strings.hasPrefix "_" f.name;
      flakeModules = lib.fileset.fileFilter isFlakeModule ./modules;
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } { imports = lib.fileset.toList flakeModules; };
}
