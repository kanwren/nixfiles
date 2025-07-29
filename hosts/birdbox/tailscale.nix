{ config, ... }: {
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "both";
    extraUpFlags = [
      "--ssh"
      "--advertise-exit-node"
    ];
    authKeyFile = config.sops.secrets."tailscale/authkey-init.txt".path;
  };

  sops.secrets."tailscale/authkey-init.txt" = {
    sopsFile = ./secrets/tailscale/authkey-init.txt;
    format = "binary";
    mode = "0440";
  };
}
