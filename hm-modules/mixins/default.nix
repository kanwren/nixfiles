{ lib, ... }:
{
  imports = [
    ./bash
    ./btop
    ./catppuccin
    ./direnv
    ./discord
    ./fish
    ./gh
    ./git
    ./gpg-agent
    ./gtk
    ./h
    ./haskell
    ./jq
    ./jujutsu
    ./k8s
    ./nix
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
