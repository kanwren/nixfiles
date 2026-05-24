{
  description = "kanwren's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs?ref=nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    catppuccin = {
      url = "github:catppuccin/nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
    };

    make-shell = {
      url = "github:nicknovitski/make-shell";
    };

    systems = {
      url = "github:nix-systems/default";
    };
  };

  outputs =
    inputs:
    let
      inherit (inputs.nixpkgs) lib;
      isFlakeModuleSrc = f: f.type == "regular" && f.hasExt "nix" && !lib.strings.hasPrefix "_" f.name;
      flakeModules = lib.fileset.toList (lib.fileset.fileFilter isFlakeModuleSrc ./modules);
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } { imports = flakeModules; };
}
