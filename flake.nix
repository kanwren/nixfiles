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
      defaultModules = [
        useFlakes
        pinFlakes
      ];
    in {
      nixosConfigurations = {
        hecate = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          modules =
            let
              args = {
                inputs = {
                  inherit
                    neovim
                    nord-dircolors
                    nord-tmux;
                };
                inherit (self) hmModules;
                nlib = self.lib;
              };
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
              addOverlays = {
                nixpkgs.overlays = [
                  nur.overlay
                  cs2110-nix.overlay.${system}
                ];
              };
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

        homepi = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules =
            let
              args = {
                inputs = {
                  inherit nix-cron;
                };
              };
              mainModule = import ./homepi/configuration.nix;
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

      hmModules = {
        xcompose = import ./hm-modules/xcompose.nix { nlib = self.lib; };
      };

      lib = import ./lib { inherit (nixpkgs) lib; };
    } // flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
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
