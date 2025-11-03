{
  config,
  pkgs,
  lib,
  ...
}:
{
  services = {
    ollama = {
      enable = false;
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
      enable = false;
      port = 8005;
      host = "127.0.0.1";
    };

    tscaddy = {
      enable = true;
      nodes =
        lib.mkIf config.services.ollama.enable {
          ollama = {
            tailnetName = "swallow-chickadee";
            target = "http://127.0.0.1:${toString config.services.ollama.port}";
            keepHost = false;
            authKeyFile = config.sops.secrets."caddy/ts-authkey-ollama".path;
            dependencies = [ "ollama.service" ];
          };
        }
        // lib.mkIf config.services.open-webui.enable {
          open-webui = {
            tailnetName = "swallow-chickadee";
            target = "http://127.0.0.1:${toString config.services.open-webui.port}";
            authKeyFile = config.sops.secrets."caddy/ts-authkey-open-webui".path;
            dependencies = [ "open-webui.service" ];
          };
        };
    };
  };

  systemd.services = lib.mkIf config.services.open-webui.enable {
    open-webui.serviceConfig.Restart = "always";
  };

  sops.secrets =
    lib.mkIf config.services.ollama.enable {
      "caddy/ts-authkey-ollama" = {
        sopsFile = ../secrets/caddy/ts-authkey-ollama.txt;
        format = "binary";
        mode = "0440";
        owner = config.services.caddy.user;
        inherit (config.services.caddy) group;
      };
    }
    // lib.mkIf config.services.open-webui.enable {
      "caddy/ts-authkey-open-webui" = {
        sopsFile = ../secrets/caddy/ts-authkey-open-webui.txt;
        format = "binary";
        mode = "0440";
        owner = config.services.caddy.user;
        inherit (config.services.caddy) group;
      };
    };
}
