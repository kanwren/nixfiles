{
  description = "nprindle's NixOS configurations and other Nix tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # pinned from nixos-22.05-aarch64
    nixpkgs-homepi.url = "github:NixOS/nixpkgs/f919a40e544da31a3b4b42e87cf30a5078c2b09c";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    flake-utils.url = "github:numtide/flake-utils";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvim-configs = {
      url = "github:nprindle/nvim-configs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-homepi
    , home-manager
    , darwin
    , nixos-hardware
    , flake-utils
    , nixos-generators
    , nixos-wsl
    , sops-nix
    , nvim-configs
    }@inputs:
    let
      recursiveMergeAttrs = nixpkgs.lib.foldl' nixpkgs.lib.recursiveUpdate { };
    in
    recursiveMergeAttrs [
      {
        nixosConfigurations = {
          # main dev laptop
          hecate = import ./hosts/hecate/host.nix inputs;

          homepi = import ./hosts/homepi/host.nix inputs;

          wsl = import ./hosts/wsl/host.nix inputs;
        };

        darwinConfigurations = {
          "nprindle-macbookpro" = import ./hosts/nprindle-macbookpro/host.nix inputs;
        };

        # Nixpkgs overlays
        overlays = import ./overlays;

        # NixOS modules
        nixosModules = import ./modules self;

        # home-manager modules
        hmModules = import ./hm-modules;

        # nix-darwin modules
        darwinModules = import ./darwin-modules { inherit nixpkgs; };

        # custom templates
        templates = import ./templates;
      }
      (flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (system: (
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [
              # formatting
              pkgs.lefthook
              self.formatter.${system}
              # sops-nix
              sops-nix.packages.${system}.sops-import-keys-hook
              sops-nix.packages.${system}.ssh-to-pgp
              sops-nix.packages.${system}.sops-init-gpg-key
            ];
            sopsPGPKeyDirs = [
              "./secrets/keys/users"
            ];
          };

          packages = flake-utils.lib.flattenTree (import ./pkgs { inherit pkgs; });

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
}
