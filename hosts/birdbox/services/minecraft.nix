{
  config,
  lib,
  ...
}:

let
  port = 25565;
in
{
  services.minecraft-ftb-server = {
    enable = true;
    eula = true;

    # https://www.feed-the-beast.com/modpacks/125-ftb-evolution
    modpackInfo = {
      id = "125";
      versionId = "100135";
    };

    serverProperties = {
      inherit port;
      enable-rcon = true;
      rcon = 25575;
      log-ips = false;

      motd = "FTB Evolution :3";

      difficulty = "normal";
      spawn-protection = 0;
      max-players = 8;

      white-list = true;
      enforce-whitelist = true;
    };

    jvmOpts = lib.concatStringsSep " " [
      # 4G RAM
      "-Xmx16G"
      "-Xms16G"
      "-XX:+UseZGC"
      "-XX:+ZGenerational"
      "-XX:+AlwaysPreTouch"
      "-XX:+UseStringDeduplication"
      "-Dsun.rmi.dgc.server.gcInterval=600000"
    ];
  };

  networking.firewall.allowedTCPPorts = [ port ];

  # Expose to tailnet behind TCP reverse proxy
  services.ts-l4-rproxy = {
    enable = true;
    nodes.ftb = {
      stateDir = "/var/lib/ts-l4-rproxy/ftb";
      environmentFile = config.sops.templates."ts-l4-rproxy.env".path;
      proxies = [
        {
          protocol = "tcp";
          listenAddr = ":25565";
          targetAddr = "127.0.0.1:${toString config.services.minecraft-ftb-server.serverProperties.port}";
        }
      ];
    };
  };

  sops = {
    templates."ts-l4-rproxy.env".content = ''
      TS_AUTHKEY=${config.sops.placeholder."minecraft-ftb-server/ts-authkey"}
    '';
    secrets."minecraft-ftb-server/ts-authkey" = {
      sopsFile = ../secrets/minecraft-ftb-server/ts-authkey.txt;
      format = "binary";
    };
  };
}
