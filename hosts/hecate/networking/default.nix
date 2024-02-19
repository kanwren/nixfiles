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
