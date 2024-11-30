{ lib, ... }:

{
  imports = [
    ./bash
    ./btop
    ./catppuccin
    ./cava
    ./direnv
    ./discord
    ./dunst
    ./firefox
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
    ./kitty
    ./nix
    ./rofi
    ./rust
    ./spotify
    ./vscode
    ./yazi
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
