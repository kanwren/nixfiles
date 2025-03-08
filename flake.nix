{
  description = "kanwren's NixOS configurations and other Nix tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-1.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixlib.follows = "nixpkgs";
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
  };

  outputs =
    { self
    , nixpkgs
    , lix-module
    , home-manager
    , nix-darwin
    , nixos-hardware
    , sops-nix
    , nixos-generators
    , catppuccin
    , fenix
    , naersk
    }:
    let
      defaultSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs
          defaultSystems
          (system:
            let
              pkgs = import nixpkgs {
                inherit system;
                overlays = [ self.overlays.default ];
              };
            in
            f pkgs);
    in
    {
      nixosConfigurations = {
        hecate = import ./hosts/hecate/host.nix {
          inherit self nixpkgs lix-module home-manager nixos-hardware sops-nix catppuccin;
        };
      };

      darwinConfigurations = {
        caspar = import ./hosts/caspar/host.nix {
          inherit self nixpkgs lix-module nix-darwin home-manager catppuccin;
        };
      };

      # Nixpkgs overlays
      overlays = {
        default = nixpkgs.lib.composeManyExtensions [
          fenix.overlays.default
          naersk.overlay
          (final: prev: import ./pkgs { pkgs = final; })
        ];

        installers = final: prev: final.callPackages ./installers { inherit nixos-generators; };
      } // import ./overlays;

      packages = forAllSystems (pkgs: {
        inherit (pkgs)
          catppuccin-twemoji-hearts
          envtpl
          frum
          generate-heart-emoji
          jj-helpers
          lipsum
          slides-full
          tfenv
          wd-fish;
      });

      nixosModules = import ./modules;

      darwinModules = import ./darwin-modules;

      hmModules = import ./hm-modules;

      # custom templates
      templates = import ./templates;

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = [
            pkgs.bashInteractive
            pkgs.just
            # formatting
            pkgs.lefthook
            self.formatter.${pkgs.system}

            # sops
            pkgs.sops
            pkgs.ssh-to-age
          ];
        };
      });

      formatter = forAllSystems (pkgs: pkgs.nixpkgs-fmt);

      checks = forAllSystems (pkgs: {
        check-format = pkgs.runCommand "check-format" { buildInputs = [ self.formatter.${pkgs.system} ]; } ''
          nixpkgs-fmt --check ${./.}
          touch "$out"
        '';
      });
    };

  nixConfig = {
    extra-substituters = [ "https://kanwren.cachix.org" ];
    extra-trusted-public-keys = [ "kanwren.cachix.org-1:uMS7ZtVOdof/PU46BAyehmNDD/P6qCGhYEvYP7X8YfE=" ];
  };
}
