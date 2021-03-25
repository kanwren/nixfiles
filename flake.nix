{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nur = {
      url = "github:nix-community/NUR";
    };

    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
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
    };

    neovim = {
      url = "github:neovim/neovim";
      flake = false;
    };

    nord-dircolors = {
      url = "github:arcticicestudio/nord-dircolors";
      flake = false;
    };

    nord-tmux = {
      url = "github:arcticicestudio/nord-tmux";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixos-hardware
    , flake-utils
    , sops-nix
    , nix-cron
    , nur, home-manager
    , cs2110-nix
    , neovim
    , nord-dircolors, nord-tmux
    }:
    let
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
        ];
      };
      # by default, we want to use and pin flakes on every machine
      defaultModules = [
        useFlakes
        pinFlakes
      ];
    in {
      nixosConfigurations = {
        # main dev laptop
        hecate = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          modules =
            let
              # extra args to pass to imported modules
              args = {
                inputs = {
                  inherit neovim nord-dircolors nord-tmux;
                };
                inherit (self) hmModules;
                nlib = self.lib;
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
                  cs2110-nix.overlay.${system}
                ];
              };
              # extra modules from the inputs
              otherModules = [
                home-manager.nixosModules.home-manager
              ];
            in nixpkgs.lib.flatten [
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
                inputs = { inherit nix-cron; };
              };
              # the main configuration
              mainModule = import ./homepi/configuration.nix;
              # extra modules from the inputs
              otherModules = [
                sops-nix.nixosModules.sops
              ];
            in nixpkgs.lib.flatten [
              (passArgs args)
              defaultModules
              mainModule
              otherModules
            ];
        };
      };

      # home-manager modules
      hmModules = {
        xcompose = import ./hm-modules/xcompose.nix { nlib = self.lib; };
      };

      # custom lib functions
      lib = import ./lib { inherit (nixpkgs) lib; };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # shell for working with sops
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            sops-nix.packages.${system}.sops-pgp-hook
            sops-nix.packages.${system}.ssh-to-pgp
            sops-nix.packages.${system}.sops-init-gpg-key
          ];
          sopsPGPKeyDirs = [
            "./secrets/keys/users"
          ];
        };
      }
    );
}
