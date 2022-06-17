{
  description = "nprindle's NixOS configurations and other Nix tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # doesn't need to upgrade; should stay constant
    nixpkgs-homepi.url = "github:NixOS/nixpkgs/61d24cba72831201efcab419f19b947cf63a2d61";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    flake-utils.url = "github:numtide/flake-utils";

    nix-bundle.url = "github:matthewbauer/nix-bundle";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nprindle/NixOS-WSL";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-utils = {
      url = "github:nprindle/nix-utils";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        naersk.follows = "naersk";
        fenix.follows = "fenix";
      };
    };

    nix-autobahn = {
      url = "github:nprindle/nix-autobahn";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        fenix.follows = "fenix";
        naersk.follows = "naersk";
      };
    };

    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        naersk.follows = "naersk";
      };
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
    , nixos-hardware
    , flake-utils
    , nix-bundle
    , nixos-wsl
    , sops-nix
    , home-manager
    , naersk
    , fenix
    , nixos-generators
    , nix-utils
    , nix-autobahn
    , nvim-configs
    , ...
    }@inputs:
    let
      # custom library
      nlib = import ./lib { inherit (nixpkgs) lib; };
    in
    nlib.attrsets.recursiveMergeAttrs [
      {
        nixosConfigurations = {
          # main dev laptop
          hecate = import ./hosts/hecate/host.nix inputs;

          homepi = import ./hosts/homepi/host.nix inputs;

          wsl = import ./hosts/wsl/host.nix inputs;
        };

        # Nixpkgs overlays
        overlays = import ./overlays;

        # NixOS modules
        nixosModules = import ./modules;

        # home-manager modules
        hmModules = {
          btop = import ./hm-modules/btop.nix;
        };

        # custom lib functions
        lib = nlib;

        # custom templates
        templates = import ./templates;

        bundlers = import ./bundlers { inherit nixpkgs nix-bundle; };
        defaultBundler = self.bundlers.arx-bundle;
      }
      (flake-utils.lib.eachDefaultSystem (system: (
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShell = pkgs.mkShell {
            nativeBuildInputs = [
              # formatting
              pkgs.lefthook
              pkgs.nixpkgs-fmt
              # sops-nix
              sops-nix.packages.${system}.sops-import-keys-hook
              sops-nix.packages.${system}.ssh-to-pgp
              sops-nix.packages.${system}.sops-init-gpg-key
            ];
            sopsPGPKeyDirs = [
              "./secrets/keys/users"
            ];
          };

          # traditional nested packages
          legacyPackages =
            let
              base = import ./pkgs {
                inherit pkgs;
                naersk = naersk.lib.${system};
                fenix = fenix.packages.${system};
              };
            in
            base;

          # flattened packages for flake
          packages = flake-utils.lib.flattenTree self.legacyPackages.${system};

          apps = {
            carbon-now = {
              type = "app";
              program = "${self.packages.${system}.carbon-now-cli}/bin/carbon-now";
            };
            rust-script = {
              type = "app";
              program = "${self.packages.${system}.rust-script}/bin/rust-script";
            };
            lipsum = {
              type = "app";
              program = "${self.packages.${system}."scripts/lipsum"}/bin/lipsum";
            };
          };
        }
      )))
      (import ./installer/installers.nix {
        inherit nixpkgs nixos-generators;
      })
      # Re-exports
      {
        inherit (nix-autobahn) packages apps;
      }
      {
        inherit (nix-utils) packages apps;
      }
      {
        inherit (nvim-configs) packages;
      }
    ];
}
