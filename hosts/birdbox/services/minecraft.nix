{
  config,
  lib,
  pkgs,
  ...
}: {
  services.minecraft-ftb-server = {
    enable = true;
    eula = true;

    # https://www.feed-the-beast.com/modpacks/125-ftb-evolution
    modpackInfo = {
      id = "125";
      versionId = "100124";
    };

    serverProperties = {
      port = 25565;
      enable-rcon = true;
      rcon = 25575;
      log-ips = false;

      motd = "FTB Evolution :3";

      difficulty = "normal";
      spawn-protection = 0;
      max-players = 8;
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

  # Expose to tailnet behind TCP reverse proxy
  systemd.services.ftb-ts-rproxy = {
    description = "TCP reverse proxy for FTB server";
    after = ["network.target"];
    wants = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = lib.escapeShellArgs [
        (lib.getExe pkgs.ts-l4-rproxy)
        "-name"
        "ftb"
        "-state-dir"
        "${config.services.minecraft-ftb-server.dataDir}/tsstate"
        "-proxy"
        "tcp,:25565,127.0.0.1:${toString config.services.minecraft-ftb-server.serverProperties.port}"
      ];
      Restart = "on-failure";
      RestartSec = 5;
      EnvironmentFile = config.sops.templates."ts-l4-rproxy.env".path;
      DynamicUser = true;
      ProtectSystem = "full";
      SystemCallArchitectures = "native";
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateTmp = true;
      PrivateDevices = true;
      RestrictNamespaces = true;
      RestrictRealtime = true;
      DevicePolicy = "closed";
      ProtectClock = true;
      ProtectHostname = true;
      ProtectProc = "invisible";
      ProtectControlGroups = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      LockPersonality = true;
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
