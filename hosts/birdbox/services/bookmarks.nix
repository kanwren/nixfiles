{config, ...}: let
  port = 19041;
in {
  services.readeck = {
    enable = true;
    settings = {
      main = {
        log_level = "info";
      };
      server = {
        address = "127.0.0.1";
        inherit port;
      };
    };
    environmentFile = config.sops.templates."readeck.env".path;
  };

  services.tscaddy = {
    enable = true;
    nodes.bookmarks = {
      host = "https://bookmarks.swallow-chickadee.ts.net";
      target = "http://127.0.0.1:${toString port}";
      authKeyFile = config.sops.secrets."caddy/ts-authkey-readeck".path;
      dependencies = ["readeck.service"];
    };
  };

  sops = {
    templates = {
      "readeck.env".content = ''
        READECK_SECRET_KEY=${config.sops.placeholder."readeck/secret-key"}
      '';
    };
    secrets = {
      "readeck/secret-key" = {
        sopsFile = ../secrets/readeck/secret-key.txt;
        format = "binary";
      };
      "caddy/ts-authkey-readeck" = {
        sopsFile = ../secrets/caddy/ts-authkey-readeck.txt;
        format = "binary";
        mode = "0440";
        owner = config.services.caddy.user;
        group = config.services.caddy.group;
      };
    };
  };
}
