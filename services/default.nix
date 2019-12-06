{ config, pkgs, ... }:

{
  imports = [
    ./x/xserver.nix
    ./compton.nix
  ];

  services = {
    # Enable the OpenSSH daemon
    openssh.enable = true;

    # Enable CUPS to print documents
    printing.enable = true;

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    # Show the NixOS manual on virtual console 8
    nixosManual = {
      showManual = true;
      ttyNumber = 8;
    };
  };
}
