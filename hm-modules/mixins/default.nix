{ lib, ... }:
{
  imports = [
    ./bash
    ./browsers
    ./btop
    ./catppuccin
    ./cava
    ./direnv
    ./discord
    ./dunst
    ./fish
    ./flameshot
    ./gh
    ./git
    ./gpg-agent
    ./gtk
    ./h
    ./haskell
    ./jq
    ./jujutsu
    ./k8s
    ./kitty
    ./nix
    ./rofi
    ./rust
    ./spotify
    ./zathura
    ./zellij
    ./zoxide
  ];

  options.mixins.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Whether to enable all home-manager mixins";
  };
}
