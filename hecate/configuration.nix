{
  imports = [
    ./boot
    ./security
    ./nix
    ./hardware
    ./time
    ./i18n
    ./services
    ./xserver
    ./users
    ./pkgs
    ./virtualisation
    ./networking
    ./home
  ];

  system.stateVersion = "21.05";

}

