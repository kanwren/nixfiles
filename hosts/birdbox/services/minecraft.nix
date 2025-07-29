{
  config,
  pkgs,
  ...
}: {
  services = {
    minecraft-server = {
      enable = true;
      package = pkgs.minecraft-server-ftb-neoforge.override {
        modpackInfo = {
          name = "ftb-evolution";
          id = "125";
        };
        modpackVersion = {
          version = "1.21.0";
          versionId = "100119";
          configHash = "sha256-/hcQEYvgL2m2AaKhiSb0BE0mMHJfwxryg1PW74l71CM=";
          neoforgeVersion = "21.1.194";
          neoforgeHash = "sha256-7Mi7ke/Ul7botfzH1GM9f/rc0DVMfGZog/fULMonmKA=";
        };
      };
      declarative = false;
      eula = true;
    };

    tscaddy = {
      enable = true;
      nodes = {
        ftb = {
          host = "https://ftb.swallow-chickadee.ts.net";
          target = "http://127.0.0.1:${toString config.services.minecraft-server.port}";
          authKeyFile = config.sops.secrets."caddy/ts-authkey-ftb".path;
          dependencies = ["minecraft-server.service"];
        };
      };
    };
  };

  sops.secrets = {
    "caddy/ts-authkey-ftb" = {
      sopsFile = ../secrets/caddy/ts-authkey-ftb.txt;
      format = "binary";
      mode = "0440";
      owner = config.services.caddy.user;
      inherit (config.services.caddy) group;
    };
  };
}
