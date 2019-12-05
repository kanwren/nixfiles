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
  };
}
