deps:

{ ... }:

{
  imports = [
    ./boot
    ./security
    (import ./nix deps)
    ./hardware
    ./time
    ./i18n
    ./services
    ./xserver
    ./users
    (import ./pkgs deps)
    ./virtualisation
    ./networking
    (import ./home deps)
  ];

  system.stateVersion = "21.03";

}

