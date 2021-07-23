{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # doesn't need to upgrade; should stay constant
    nixpkgs-homepi.url = "github:NixOS/nixpkgs/1c16013bd6e94da748b41cc123c6b509a23eb440";

    nur.url = "github:nix-community/NUR";

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

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-cron = {
      url = "github:nprindle/nix-cron";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cs2110-nix = {
      url = "github:nprindle/cs2110-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-autobahn = {
      url = "github:nprindle/nix-autobahn";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
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
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-homepi
    , nixos-hardware
    , flake-utils
    , nix-bundle
    , sops-nix
    , nix-cron
    , nur
    , home-manager
    , naersk
    , fenix
    , cs2110-nix
    , nixos-generators
    , nix-autobahn
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
          hecate = import ./hosts/hecate/host.nix { inherit nlib; } inputs;

          # raspberry pi for home-assistant
          homepi = import ./hosts/homepi/host.nix { inherit nlib; } inputs;
        };

        # Nixpkgs overlays
        overlays = {
          raspi-firmware-overlay = import ./overlays/firmwareLinuxNonfree.nix;
          asciiquarium-nicer-sharks = import ./overlays/asciiquarium-nicer-sharks.nix;
        };

        # NixOS modules
        nixosModules = {
          duckdns = import ./modules/duckdns.nix { inherit nix-cron; };
        };

        # home-manager modules
        hmModules = {
          xcompose = import ./hm-modules/xcompose.nix { inherit nlib; };
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
                inherit pkgs nur;
                naersk = naersk.lib.${system};
                fenix = fenix.packages.${system};
              };
              installers = {
                # custom installers via nixos-generators; we explicitly do not
                # recurseIntoAttrs to prevent them being put in 'packages'
                installer = import ./installer/installers.nix {
                  inherit nixpkgs system nixos-generators;
                  inherit (nixpkgs) lib;
                };
              };
            in
            base // installers;

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
      # Re-exports
      {
        inherit (nix-autobahn) packages apps;
      }
    ];
}
