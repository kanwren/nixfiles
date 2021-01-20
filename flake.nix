{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

    nur.url = github:nix-community/NUR;

    home-manager = {
      url = github:rycee/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = github:NixOS/nixos-hardware;

    flake-utils.url = github:numtide/flake-utils;

    sops-nix = {
      url = github:Mic92/sops-nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-cron = {
      url = github:nprindle/nix-cron;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cs2110-nix.url = github:nprindle/cs2110-nix;

    neovim = {
      url = github:neovim/neovim;
      flake = false;
    };

    nord-dircolors = {
      url = github:arcticicestudio/nord-dircolors;
      flake = false;
    };

    nord-tmux = {
      url = github:arcticicestudio/nord-tmux;
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
    in {
      nixosConfigurations = {
        hecate = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          modules = [
            # nixos-hardware modules
            nixos-hardware.nixosModules.common-pc-laptop
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            nixos-hardware.nixosModules.common-cpu-amd
            nixos-hardware.nixosModules.common-gpu-nvidia
            # configure the bus IDs for common-gpu-nvidia
            {
              hardware.nvidia.prime = {
                intelBusId = "PCI:5:0:0";
                nvidiaBusId = "PCI:1:0:0";
              };
            }
            # the main configuration
            (import ./hecate/configuration.nix {
              inherit neovim nord-dircolors nord-tmux;
              inherit (self.hmModules) xcompose;
            })
            # pin flakes and nixpkgs
            pinFlakes
            # add overlays to system nixpkgs
            {
              nixpkgs.overlays = [
                nur.overlay
                cs2110-nix.overlay.${system}
              ];
            }
            # other nixos modules
            home-manager.nixosModules.home-manager
          ];
        };

        homepi = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (import ./homepi/configuration.nix { inherit nix-cron; })
            pinFlakes
            sops-nix.nixosModules.sops
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
          ];
          sopsPGPKeyDirs = [
            "./secrets/keys/users"
          ];
        };
      }
    );
}
