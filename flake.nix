{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

    nur.url = github:nix-community/NUR;

    home-manager.url = github:rycee/home-manager;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

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
      };
      addOverlays = {
        nixpkgs.overlays = [
          nur.overlay
        ];
      };
    in {
      nixosConfigurations.nprin-tufa17 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          (import ./configuration.nix { inherit neovim nord-dircolors nord-tmux; })
          pinFlakes
          addOverlays
          home-manager.nixosModules.home-manager
        ];
      };
    };
}
