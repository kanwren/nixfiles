{ config, ... }:

{
  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  services.open-webui = {
    enable = true;
    port = 8005;
    host = "127.0.0.1";
  };
  systemd.services.open-webui.serviceConfig.Restart = "always";

  services.tscaddy.nodes.open-webui = {
    host = "https://open-webui.swallow-chickadee.ts.net";
    target = "http://127.0.0.1:${toString config.services.open-webui.port}";
    authKeyFile = config.sops.secrets."caddy/open-webui-ts-authkey".path;
    dependencies = [ "open-webui.service" ];
  };
  sops.secrets."caddy/open-webui-ts-authkey" = {
    sopsFile = ../secrets/caddy-ts-authkey-open-webui.txt;
    format = "binary";
    mode = "0440";
    owner = config.services.caddy.user;
    group = config.services.caddy.group;
  };
}
