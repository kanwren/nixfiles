{ nix-cron }:

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.duckdns;
  inherit (nix-cron.lib) cron;
in
{
  options = {
    services.duckdns = {
      enable = mkEnableOption "Duck DNS";

      token = mkOption {
        type = with lib.types; nullOr str;
        default = null;
        description = ''
          The Duck DNS token to use for updates. Note that this token is put
          into the Nix store. Use <literal>tokenPath</literal> if this is
          undesirable.
        '';
      };

      tokenPath = mkOption {
        type = with lib.types; nullOr path;
        default = null;
        description = ''
          A string path to a file containing the Duck DNS token to use. The path
          need not be in the Nix store, as long as it is available at runtime.
        '';
      };

      subdomain = mkOption {
        type = with lib.types; str;
        description = ''
          The subdomain to use; the part of the URL before
          <literal>duckdns.org</literal>.
        '';
      };

      time = mkOption {
        type = with cron.types; time_t;
        default = with cron; { minutes = every 5; };
        description = ''
          A string representing a cron configuration for how often to schedule a
          DNS update. By default, updates every 5 minutes.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = (cfg.token == null) != (cfg.tokenPath == null);
        description = "Either services.duckdns.token or services.duckdns.tokenPath must be set";
      }
    ];

    services.cron = {
      enable = true;
      systemCronJobs =
        let
          updateDuckIp = pkgs.writeShellScript "update-duck-ip" ''
            ${
              if cfg.token != null then ''
                url="https://www.duckdns.org/update?domains=${cfg.subdomain}&token=${cfg.token}&verbose=true&ip="
              '' else ''
                token="$(< ${cfg.tokenPath})"
                url="https://www.duckdns.org/update?domains=${cfg.subdomain}&token=$token&verbose=true&ip="
              ''
            }
            echo url="$url" | curl -k -o /tmp/duck.log -K - >/dev/null 2>&1
          '';
          updateJob = cron.systemJob {
            time = cfg.time;
            user = "root";
            commandFile = updateDuckIp;
          };
        in
        [ updateJob ];
    };
  };
}
