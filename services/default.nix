{ config, pkgs, ... }:

{
  imports = [
    ./xserver.nix
    ./compton.nix
  ];

  services = {
    # Enable the OpenSSH daemon
    openssh.enable = true;

    # Enable CUPS to print documents
    printing.enable = true;

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;
  };
}
