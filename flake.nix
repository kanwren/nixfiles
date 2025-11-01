{
  description = "kanwren's NixOS configurations and other Nix tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

    impermanence = {
      url = "github:nix-community/impermanence";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      rev = self.rev or self.dirtyRev or null;
      defaultSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs defaultSystems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      nixosConfigurations = {
        hecate = import ./hosts/hecate/host.nix { inherit (self) inputs outputs; };
        birdbox = import ./hosts/birdbox/host.nix { inherit (self) inputs outputs; };
      };

      darwinConfigurations = {
        nightjar = import ./hosts/nightjar/host.nix { inherit (self) inputs outputs; };
      };

      overlays = import ./overlays { inherit (self) inputs outputs; };

      nixosModules = import ./modules;
      darwinModules = import ./darwin-modules;
      hmModules = import ./hm-modules;

      templates = import ./templates;

      lib = import ./lib {
        inherit (self) inputs;
        inherit rev;
      };

      packages = forAllSystems (pkgs: {
        inherit (pkgs.extend self.overlays.additions)
          catppuccin-twemoji-hearts
          envtpl
          frum
          generate-heart-emoji
          lipsum
          slides-full
          tfenv
          wd-fish
          caddy-with-plugins
          ftb-server-installer
          ;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = [
            pkgs.bashInteractive
            pkgs.just
            pkgs.jq

            # formatting
            self.formatter.${pkgs.stdenv.hostPlatform.system}

            # sops
            pkgs.sops
            pkgs.ssh-to-age
          ];
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);

      checks = forAllSystems (pkgs: {
        check-format = pkgs.runCommand "check-format" { buildInputs = [ pkgs.nixfmt-tree ]; } ''
          treefmt --ci ${./.} && touch "$out"
        '';
      });
    };

  nixConfig = { };
}
