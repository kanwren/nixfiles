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

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence = {
      url = "github:nix-community/impermanence";
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-stable
    , home-manager
    , nix-darwin
    , nixos-hardware
    , sops-nix
    , nixos-generators
    , catppuccin
    , fenix
    , naersk
    , disko
    , impermanence
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
          inherit self nixpkgs home-manager nixos-hardware sops-nix catppuccin;
        };

        birdbox = import ./hosts/birdbox/host.nix {
          inherit self nixpkgs home-manager nixos-hardware sops-nix disko impermanence catppuccin;
        };
      };

      darwinConfigurations = {
        caspar = import ./hosts/caspar/host.nix {
          inherit self nixpkgs nix-darwin home-manager catppuccin;
        };
      };

      # Nixpkgs overlays
      overlays = {
        default = nixpkgs.lib.composeManyExtensions [
          fenix.overlays.default
          naersk.overlays.default
          (final: prev: import ./pkgs { pkgs = final; })
        ];

        installers = final: prev: final.callPackages ./installers { inherit nixos-generators; };

        fix-open-webui = final: prev: {
          inherit (nixpkgs-stable.legacyPackages.${prev.system})
            open-webui;
        };
      } // import ./overlays;

      packages = forAllSystems (pkgs: {
        inherit (pkgs)
          catppuccin-twemoji-hearts
          envtpl
          frum
          generate-heart-emoji
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
