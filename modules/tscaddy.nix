# Configure caddy for running virtual tailnet services via caddy-tailscale
{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.services.tscaddy;
  nodes = builtins.attrValues cfg.nodes;
in {
  options.services.tscaddy = {
    enable = lib.mkEnableOption "Enable Caddy virtual tailnet services";

    nodes = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({
        name,
        lib,
        ...
      }: {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            default = name;
            description = ''
              The name of the service's tailnet node
            '';
          };

          host = lib.mkOption {
            type = lib.types.str;
            example = "https://my-service-name.tail-scale.ts.net";
            description = ''
              The full hostname for the service
            '';
          };

          target = lib.mkOption {
            type = lib.types.str;
            example = "http://127.0.0.1:8080";
            description = ''
              The (usually local) host that Caddy should proxy to
            '';
          };

          authKeyFile = lib.mkOption {
            type = lib.types.path;
            description = ''
              The path to the auth key file for the service
            '';
          };

          dependencies = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [];
            description = ''
              Extra systemd services that Caddy should depend on
            '';
          };
        };
      }));
      default = {};
      description = ''
        Specification of virtual tailnet services to proxy
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.caddy = {
      enable = true;
      package = pkgs.callPackage ./caddy {};
      globalConfig = let
        configForService = service: ''
          ${service.name} {
            auth_key {file.${service.authKeyFile}}
            state_dir ${config.services.caddy.dataDir}/ts-state_${service.name}
          }
        '';
      in ''
        tailscale {
        ${lib.strings.concatMapStringsSep "\n" configForService nodes}
        }
      '';
      virtualHosts = let
        configureService = opts: {
          name = opts.host;
          value = {
            extraConfig = ''
              bind tailscale/${opts.name}
              tailscale_auth
              reverse_proxy ${opts.target}
            '';
          };
        };
      in
        builtins.listToAttrs (builtins.map configureService nodes);
    };

    systemd.services.caddy = let
      deps = builtins.concatMap (x: x.dependencies) nodes;
    in {
      wants = ["tailscaled.service" "sops-nix.service"] ++ deps;
      after = ["tailscaled.service" "sops-nix.service"] ++ deps;
      serviceConfig.Restart = "on-failure";
    };
  };
}
