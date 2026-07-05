{
  flake.modules.nixos.base = {
    networking = {
      firewall.enable = true;
      networkmanager = {
        enable = true;
        dns = "systemd-resolved";
      };
    };

    services.resolved.enable = true;
  };
}
