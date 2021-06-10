{
  inputs = {
    # Flakes
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-utils.url = "github:numtide/flake-utils";
    nix-bundle.url = "github:matthewbauer/nix-bundle";
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
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-autobahn = {
      url = "github:nprindle/nix-autobahn";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    gytis = {
      url = "github:gytis-ivaskevicius/nixfiles";
      inputs.nixpkgs.follows = "nixpkgs";
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
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
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
    , gytis
    , neovim-nightly-overlay
    , ...
    }@inputs:
    let
      # custom library
      nlib = import ./lib { inherit (nixpkgs) lib; };
      # Module to pass extra arguments to modules
      passArgs = args: {
        config._module.args = args;
      };
      # Module to enable nix flakes
      useFlakes = { pkgs, ... }: {
        nix = {
          package = pkgs.nixFlakes;
          extraOptions = ''
            experimental-features = nix-command flakes ca-references
          '';
        };
      };
      # pin flakes and nixpkgs
      pinFlakes = {
        nix.registry = {
          nixpkgs.flake = nixpkgs;
          nur.flake = nur;
          home-manager.flake = home-manager;
        };
        nix.nixPath = [
          "nixpkgs=${nixpkgs}"
          "nur=${nur}"
        ];
      };
      # by default, we want to use and pin flakes on every machine
      defaultModules = [
        useFlakes
        pinFlakes
      ];
    in
    nlib.attrsets.recursiveMergeAttrs [
      {
        nixosConfigurations = {
          # main dev laptop
          hecate = nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            modules =
              let
                # extra args to pass to imported modules
                args = {
                  inherit inputs;
                  custom = {
                    pkgs = self.legacyPackages.${system};
                    inherit (self) hmModules;
                    inherit (self) lib;
                  };
                };
                # modules for configuring hecate hardware
                hardwareModules = with nixos-hardware.nixosModules; [
                  common-pc-laptop
                  common-pc-laptop-ssd
                  common-cpu-amd
                  common-gpu-nvidia
                  # configure the bus IDs for common-gpu-nvidia
                  {
                    hardware.nvidia.prime = {
                      intelBusId = "PCI:5:0:0";
                      nvidiaBusId = "PCI:1:0:0";
                    };
                  }
                  # Auto-generated hardware configuration
                  ./hecate/hardware-configuration.nix
                ];
                # the main configuration
                mainModule = import ./hecate/configuration.nix;
                # extra overlays from the inputs
                addOverlays = {
                  nixpkgs.overlays = [
                    nur.overlay
                    cs2110-nix.overlay
                    gytis.overlay
                    neovim-nightly-overlay.overlay
                  ];
                };
                # extra modules from the inputs
                otherModules = [
                  home-manager.nixosModules.home-manager
                ];
              in
              nixpkgs.lib.flatten [
                (passArgs args)
                defaultModules
                hardwareModules
                mainModule
                addOverlays
                otherModules
              ];
          };

          # raspberry pi for home-assistant
          homepi = nixpkgs.lib.nixosSystem {
            system = "aarch64-linux";
            modules =
              let
                # extra args to pass to imported modules
                args = {
                  inherit inputs;
                };
                # the main configuration
                mainModule = import ./homepi/configuration.nix;
                # extra modules from the inputs
                otherModules = [
                  sops-nix.nixosModules.sops
                  self.nixosModules.duckdns
                ];
                addOverlays = {
                  nixpkgs.overlays = [
                    self.overlays.raspi-firmware-overlay
                  ];
                };
              in
              nixpkgs.lib.flatten [
                (passArgs args)
                defaultModules
                mainModule
                addOverlays
                otherModules
              ];
          };
        };

        # Nixpkgs overlays
        overlays = {
          raspi-firmware-overlay = import ./overlays/firmwareLinuxNonfree.nix;
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
        templates = {
          latex = {
            path = ./templates/latex;
            description = "A basic LaTeX project";
          };
        };

        bundlers = {
          # nix-bundle shim to bundle programs with names other than the default
          # See https://github.com/matthewbauer/nix-bundle/issues/74
          arx-bundle = { program, system }:
            let
              pkgs = nixpkgs.legacyPackages.${system};
              bundle = import nix-bundle { nixpkgs = pkgs; };
              envProg = builtins.getEnv "PROGRAM";
              prog =
                if envProg == "" then
                  builtins.trace "Warning: PROGRAM not set; defaulting to '${program}'. Did you forget to set PROGRAM or --impure?" program
                else
                  "${builtins.dirOf program}/${envProg}";
              script = pkgs.writeScript "startup" ''
                #!/bin/sh
                .${bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -- "${prog}" "$@"
              '';
            in
            bundle.makebootstrap {
              targets = [ script ];
              startup = ".${builtins.unsafeDiscardStringContext script} '\"$@\"'";
            };

          docker-bundle = { program, system }:
            let
              pkgs = nixpkgs.legacyPackages.${system};
              envProg = builtins.getEnv "PROGRAM";
              prog =
                if envProg == "" then
                  builtins.trace "Warning: PROGRAM not set; defaulting to '${program}'. Did you forget to set PROGRAM or --impure?" program
                else
                  "${builtins.dirOf program}/${envProg}";
            in
            pkgs.dockerTools.buildImage {
              name = "${builtins.baseNameOf program}";
              tag = "latest";
              config.Cmd = [ prog ];
            };
        };

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
              sops-nix.packages.${system}.sops-pgp-hook
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
