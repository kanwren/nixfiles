{
  config,
  pkgs,
  ...
}:
{
  services = {
    ollama = {
      enable = true;
      package = pkgs.ollama-cuda.override {
        cudaArches = [ "61" ];
      };
      acceleration = "cuda";
      port = 11434;
      host = "127.0.0.1";
      loadModels = [
        "qwen2.5-coder:1.5b"
        "qwen2.5-coder:3b"
        "qwen2.5-coder:7b"
      ];
    };

    open-webui = {
      enable = true;
      port = 8005;
      host = "127.0.0.1";
    };

    tscaddy = {
      enable = true;
      nodes = {
        ollama = {
          tailnetName = "swallow-chickadee";
          target = "http://127.0.0.1:${toString config.services.ollama.port}";
          keepHost = false;
          authKeyFile = config.sops.secrets."caddy/ts-authkey-ollama".path;
          dependencies = [ "ollama.service" ];
        };
        open-webui = {
          tailnetName = "swallow-chickadee";
          target = "http://127.0.0.1:${toString config.services.open-webui.port}";
          authKeyFile = config.sops.secrets."caddy/ts-authkey-open-webui".path;
          dependencies = [ "open-webui.service" ];
        };
      };
    };
  };

  systemd.services.open-webui.serviceConfig.Restart = "always";

  sops.secrets = {
    "caddy/ts-authkey-ollama" = {
      sopsFile = ../secrets/caddy/ts-authkey-ollama.txt;
      format = "binary";
      mode = "0440";
      owner = config.services.caddy.user;
      inherit (config.services.caddy) group;
    };
    "caddy/ts-authkey-open-webui" = {
      sopsFile = ../secrets/caddy/ts-authkey-open-webui.txt;
      format = "binary";
      mode = "0440";
      owner = config.services.caddy.user;
      inherit (config.services.caddy) group;
    };
  };
}
