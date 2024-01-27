{
  description = "kanwren's NixOS configurations and other Nix tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    flake-utils.url = "github:numtide/flake-utils";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    catppuccin-btop-src = {
      url = "github:catppuccin/btop";
      flake = false;
    };
    catppuccin-cava-src = {
      url = "github:catppuccin/cava";
      flake = false;
    };
    catppuccin-kitty-src = {
      url = "github:catppuccin/kitty";
      flake = false;
    };
    catppuccin-k9s-src = {
      url = "github:catppuccin/k9s";
      flake = false;
    };
    catppuccin-tmux-src = {
      url = "github:catppuccin/tmux";
      flake = false;
    };
    catppuccin-zathura-src = {
      url = "github:catppuccin/zathura";
      flake = false;
    };
    k8split-src = {
      url = "github:brendanjryan/k8split";
      flake = false;
    };
    envtpl-src = {
      url = "github:subfuzion/envtpl";
      flake = false;
    };

    wd-fish-src = {
      url = "github:fischerling/plugin-wd";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , nix-darwin
    , nixos-hardware
    , flake-utils
    , nixos-generators
    , catppuccin-btop-src
    , catppuccin-cava-src
    , catppuccin-kitty-src
    , catppuccin-k9s-src
    , catppuccin-tmux-src
    , catppuccin-zathura-src
    , k8split-src
    , envtpl-src
    , wd-fish-src
    }@inputs:
    let
      recursiveMergeAttrs = nixpkgs.lib.foldl' nixpkgs.lib.recursiveUpdate { };
      sources = {
        inherit (inputs)
          catppuccin-btop-src
          catppuccin-cava-src
          catppuccin-kitty-src
          catppuccin-k9s-src
          catppuccin-tmux-src
          catppuccin-zathura-src
          k8split-src
          envtpl-src
          wd-fish-src;
      };
    in
    recursiveMergeAttrs [
      {
        nixosConfigurations = {
          # main dev laptop
          hecate = import ./hosts/hecate/host.nix inputs;
        };

        darwinConfigurations = {
          caspar = import ./hosts/caspar/host.nix inputs;
        };

        # Nixpkgs overlays
        overlays = import ./overlays;

        # NixOS modules
        nixosModules = import ./modules { inherit self; };

        # home-manager modules
        hmModules = import ./hm-modules { inherit self; };

        # custom templates
        templates = import ./templates;
      }
      (flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (system: (
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = pkgs.mkShellNoCC {
            packages = [
              pkgs.just
              # formatting
              pkgs.lefthook
              self.formatter.${system}
            ];
          };

          packages = import ./pkgs { inherit pkgs sources; };

          formatter = pkgs.nixpkgs-fmt;

          checks.check-format = pkgs.runCommand "check-format" { buildInputs = [ self.formatter.${system} ]; } ''
            nixpkgs-fmt --check ${./.}
            touch "$out"
          '';
        }
      )))
      (import ./installer/installers.nix {
        inherit nixpkgs nixos-generators;
      })
    ];

  nixConfig = {
    extra-substituters = [ "https://kanwren.cachix.org" ];
    extra-trusted-public-keys = [ "kanwren.cachix.org-1:uMS7ZtVOdof/PU46BAyehmNDD/P6qCGhYEvYP7X8YfE=" ];
  };
}
