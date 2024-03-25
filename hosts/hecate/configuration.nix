{ pkgs, ... }:

{
  imports = [
    ./boot
    ./hardware
    ./i18n
    ./networking
    ./nix
    ./pkgs
    ./security
    ./services
    ./shells
    ./tailscale
    ./time
    ./users
    ./virtualisation
    ./x
  ];
}
