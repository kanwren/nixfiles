{
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "both";
    extraUpFlags = [ "--ssh" ];
  };
  networking.firewall = {
    trustedInterfaces = [ "tailscale0" ];
  };
}
