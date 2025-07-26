{
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "both";
    extraUpFlags = [
      "--ssh"
      "--advertise-exit-node"
    ];
  };

  networking.firewall = {
    trustedInterfaces = ["tailscale0"];
  };

  services.tscaddy.enable = true;
}
