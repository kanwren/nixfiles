{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.minecraft-ftb-server;

  eulaFile = builtins.toFile "eula.txt" ''
    eula=true
  '';

  serverPropertiesFile =
    let
      renderValue = v: if builtins.isBool v then lib.boolToString v else toString v;
      renderEntry = k: v: "${k}=${renderValue v}";
      renderProperties = p: lib.concatStringsSep "\n" (lib.mapAttrsToList renderEntry p);
    in
    pkgs.writeText "server.properties" (renderProperties cfg.serverProperties);
in
{
  options = {
    services.minecraft-ftb-server = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          If enabled, start a Minecraft Server.
        '';
      };

      eula = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether you agree to [Mojangs EULA](https://www.minecraft.net/eula).
          This option must be set to `true` to run the Minecraft server.
        '';
      };

      dataDir = lib.mkOption {
        type = lib.types.path;
        default = "/var/lib/minecraft-ftb";
        description = ''
          Directory to store Minecraft database and other state/data files.
        '';
      };

      modpackInfo.id = lib.mkOption {
        type = lib.types.str;
        example = "123";
        description = ''
          The ID of the modpack; for example, 125 for FTB Evolution. You can find this under "Pack ID" on the modpack page.
        '';
      };

      modpackInfo.versionId = lib.mkOption {
        type = lib.types.str;
        example = "100123";
        description = ''
          The ID of the version of the modpack to use. This is the "ID" chip next to the version in the "Select Version" dropdown.
        '';
      };

      jvmOpts = lib.mkOption {
        type = lib.types.separatedString " ";
        default = "-Xmx2048M -Xms2048M";
        example =
          "-Xms4092M -Xmx4092M -XX:+UseG1GC -XX:+CMSIncrementalPacing "
          + "-XX:+CMSClassUnloadingEnabled -XX:ParallelGCThreads=2 "
          + "-XX:MinHeapFreeRatio=5 -XX:MaxHeapFreeRatio=10";
        description = "JVM options for the Minecraft server.";
      };

      serverProperties = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.oneOf [
            lib.types.str
            lib.types.int
            lib.types.bool
          ]
        );
        default = { };
        description = ''
          A set of properties to be written to the `server.properties` file.
          These properties are applied at every launch; any property not
          specified will be managed mutably.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.minecraft-ftb = {
      description = "minecraft-ftb server service user";
      home = cfg.dataDir;
      createHome = true;
      isSystemUser = true;
      group = "minecraft-ftb";
    };
    users.groups.minecraft-ftb = { };

    systemd.sockets.minecraft-ftb-server = {
      bindsTo = [ "minecraft-ftb-server.service" ];
      socketConfig = {
        ListenFIFO = "/run/minecraft-ftb-server.stdin";
        SocketMode = "0660";
        SocketUser = "minecraft-ftb";
        SocketGroup = "minecraft-ftb";
        RemoveOnStop = true;
        FlushPending = true;
      };
    };

    systemd.services.minecraft-ftb-server = {
      description = "Minecraft Server Service";
      wantedBy = [ "multi-user.target" ];
      requires = [ "minecraft-ftb-server.socket" ];
      after = [
        "network.target"
        "minecraft-ftb-server.socket"
      ];

      preStart = ''
        set -euo pipefail

        ln -sf ${eulaFile} eula.txt

        echo ${lib.escapeShellArg cfg.jvmOpts} > user_jvm_args.txt

        # We append to the server properties file so that the specified
        # defaults take precedence over what's already present. The Minecraft
        # server parses and rerenders this file on startup, normalizing config
        # and setting all missing options.
        cat ${serverPropertiesFile} >> server.properties

        manifest_matches() {
          local id="$1"
          local version_id="$2"
          test -f .manifest.json \
          && ${lib.escapeShellArg (lib.getExe pkgs.jq)} \
            --argjson id "$id" \
            --argjson version_id "$version_id" \
            --exit-status \
            '.id == $id and .versionId == $version_id' \
            .manifest.json \
            >/dev/null 2>/dev/null
        }

        if ! manifest_matches ${
          lib.escapeShellArgs [
            cfg.modpackInfo.id
            cfg.modpackInfo.versionId
          ]
        }; then
          echo 'Manifest does not match; running installer...'
          ${lib.escapeShellArg (lib.getExe pkgs.ftb-server-installer)} -auto -dir . -force -no-java -no-colours -pack ${cfg.modpackInfo.id} -version ${cfg.modpackInfo.versionId}
        fi
      '';

      path = [ pkgs.jre_headless ];

      environment = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
        LD_LIBRARY_PATH = lib.makeLibraryPath [ pkgs.udev ];
      };

      serviceConfig = {
        TimeoutStartSec = "5min";
        ExecStart = pkgs.writeShellScript "start" (
          lib.escapeShellArgs [
            (lib.getExe pkgs.bash)
            "./run.sh"
            "--nogui"
          ]
        );
        ExecStop =
          let
            stopScript = pkgs.writeShellScript "minecraft-ftb-server-stop" ''
              echo stop > ${config.systemd.sockets.minecraft-ftb-server.socketConfig.ListenFIFO}
              while kill -0 "$1" 2>/dev/null; do
                sleep 1s
              done
            '';
          in
          "${stopScript} $MAINPID";
        Restart = "always";
        User = "minecraft-ftb";
        WorkingDirectory = cfg.dataDir;
        StandardInput = "socket";
        StandardOutput = "journal";
        StandardError = "journal";

        CapabilityBoundingSet = [ "" ];
        DeviceAllow = [ "" ];
        LockPersonality = true;
        PrivateDevices = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        UMask = "0077";
      };
    };

    assertions = [
      {
        assertion = cfg.eula;
        message = "You must agree to Mojangs EULA to run minecraft-ftb-server.";
      }
    ];
  };
}
