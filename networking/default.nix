{ config, pkgs, ... }:

{
  networking = {

    hostName = "nprin";

    networkmanager = {
      enable = true;
    };

    wireless = {
      enable = false;
    };

    useDHCP = false;
    interfaces.eno1.useDHCP = true;
    interfaces.wlo1.useDHCP = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

  };
}
