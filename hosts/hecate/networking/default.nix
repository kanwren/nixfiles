{ ... }:

{
  networking = {

    hostName = "hecate";

    networkmanager.enable = true;
    wireless.enable = false;

    useDHCP = false;
    # Obtain list of interfaces at /sys/class/net/*
    interfaces = {
      enp2s0.useDHCP = true;
      wlp3s0.useDHCP = true;
    };

    firewall = {
      enable = true;
      allowedTCPPorts = [ 631 ];
      allowedUDPPorts = [ 631 ];
    };

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

  };
}
