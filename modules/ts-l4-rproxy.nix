{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.services.ts-l4-rproxy;
  nodes = builtins.attrValues cfg.nodes;
in
{
  options.services.ts-l4-rproxy = {
    enable = lib.mkEnableOption "Enable tsnet TCP/UDP reverse proxy";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ts-l4-rproxy;
      description = ''
        The package to use for the service
      '';
    };

    nodes = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          {
            name,
            lib,
            config,
            ...
          }:
          {
            options = {
              name = lib.mkOption {
                type = lib.types.str;
                default = name;
                description = ''
                  The name of the service's tailnet node
                '';
              };

              stateDir = lib.mkOption {
                type = lib.types.path;
                default = "/var/lib/ts-l4-rproxy/${config.name}";
                description = ''
                  The directory to store the tsnet state in
                '';
              };

              environmentFile = lib.mkOption {
                type = lib.types.path;
                description = ''
                  The path to the environment variables for the service. Use this to
                  pass TS_AUTHKEY.
                '';
              };

              proxies = lib.mkOption {
                type = lib.types.listOf (
                  lib.types.submodule (
                    { lib, ... }:
                    {
                      options = {
                        protocol = lib.mkOption {
                          type = lib.types.enum [
                            "tcp"
                            "udp"
                          ];
                          description = ''
                            The protocol to proxy
                          '';
                        };

                        listenAddr = lib.mkOption {
                          type = lib.types.str;
                          example = ":8080";
                          description = ''
                            The address on the tsnet node to listen on
                          '';
                        };

                        targetAddr = lib.mkOption {
                          type = lib.types.str;
                          example = "127.0.0.1:8080";
                          description = ''
                            The (usually local) host to proxy to
                          '';
                        };
                      };
                    }
                  )
                );
                default = [ ];
                description = ''
                  List of proxies to configure for this service
                '';
              };
            };
          }
        )
      );
      default = { };
      description = ''
        Specification of virtual tailnet services to proxy
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services =
      let
        configureService =
          c:
          lib.nameValuePair "ts-l4-rproxy-${c.name}" {
            description = "tsnet TCP/UDP reverse proxy for ${c.name}";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            serviceConfig = {
              Type = "simple";
              ExecStart = lib.escapeShellArgs (
                [
                  (lib.getExe cfg.package)
                  "-name"
                  c.name
                  "-state-dir"
                  c.stateDir
                ]
                ++ lib.forEach c.proxies (
                  p:
                  lib.concatStringsSep "," [
                    p.protocol
                    p.listenAddr
                    p.targetAddr
                  ]
                )
              );
              Restart = "on-failure";
              RestartSec = 5;
              EnvironmentFile = c.environmentFile;
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
      in
      lib.mapAttrs' (_: configureService) cfg.nodes;
  };
}
