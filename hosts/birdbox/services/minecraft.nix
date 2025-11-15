{ lib, ... }:

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
      allow-flight = true;

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
}
