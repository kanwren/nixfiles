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

      white-list = true;
      enforce-whitelist = true;
    };

    jvmOpts = lib.concatStringsSep " " [
      # 4G RAM
      "-Xmx4G"
      "-Xms4G"

      # G1 garbage collector tuning
      "-XX:+UseG1GC"
      "-Dsun.rmi.dgc.server.gcInterval=600000"
      "-XX:+UnlockExperimentalVMOptions"
      "-XX:+DisableExplicitGC"
      "-XX:G1NewSizePercent=20"
      "-XX:G1ReservePercent=20"
      "-XX:MaxGCPauseMillis=50"
      "-XX:G1HeapRegionSize=32"
    ];
  };

  networking.firewall.allowedTCPPorts = [ port ];
}
