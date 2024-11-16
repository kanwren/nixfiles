{
  networking = {
    networkmanager = {
      enable = true;
    };
    wireless.enable = false;

    # DHCP should be enabled for each interface individually via
    # interfaces.<name>.useDHCP = true;
    #
    # The list of interfaces can be obtained in /sys/class/net/*
    useDHCP = false;
    interfaces = {
      enp2s0.useDHCP = true;
      wlp3s0.useDHCP = true;
    };

    firewall = {
      enable = true;
      allowedTCPPorts = [ 5001 8001 8002 ];
      allowedUDPPorts = [ ];
    };

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];
  };
}
