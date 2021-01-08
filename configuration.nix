{ neovim, nord-dircolors, nord-tmux }:

{ ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./boot
    ./security
    (import ./nix { inherit neovim; })
    ./hardware
    ./time
    ./i18n
    ./services
    ./xserver
    ./users
    (import ./pkgs { inherit nord-dircolors; })
    ./virtualisation
    ./networking
    (import ./home { inherit nord-tmux; })
  ];

  system.stateVersion = "20.09";

}

