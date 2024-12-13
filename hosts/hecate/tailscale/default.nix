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
    trustedInterfaces = [ "tailscale0" ];
  };

  systemd.services.tailscaled.after = [ "systemd-networkd-wait-online.service" ];
}
