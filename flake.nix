{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

    nur.url = github:nix-community/NUR;

    home-manager.url = github:rycee/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = github:numtide/flake-utils;

    sops-nix.url = github:Mic92/sops-nix;
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";

    nix-cron.url = github:nprindle/nix-cron;
    nix-cron.inputs.nixpkgs.follows = "nixpkgs";

    # cs2110-nix.url = github:nprindle/cs2110-nix;

    neovim.url = github:neovim/neovim;
    neovim.flake = false;

    nord-dircolors.url = github:arcticicestudio/nord-dircolors;
    nord-dircolors.flake = false;

    nord-tmux.url = github:arcticicestudio/nord-tmux;
    nord-tmux.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , sops-nix
    , nix-cron
    , nur, home-manager
    # , cs2110-nix
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
      addOverlays = {
        nixpkgs.overlays = [
          nur.overlay
        ];
      };
    in {
      nixosConfigurations = {
        hecate = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          modules = [
            (import ./hecate/configuration.nix { inherit neovim nord-dircolors nord-tmux; })
            pinFlakes
            addOverlays
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
