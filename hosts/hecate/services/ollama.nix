{
  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  services.open-webui = {
    enable = true;
    port = 8005;
    host = "0.0.0.0";
  };
  systemd.services.open-webui.serviceConfig.Restart = "always";
}
