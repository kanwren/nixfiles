{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.services.duckdns;
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

      onCalendar = mkOption {
        type = with lib.types; str;
        default = "*-*-* *:00:00";
        description = ''
          A string representing a systemd OnCalendar configuration for how often
          to schedule a DNS update. By default, updates once an hour.
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

    systemd = {
      services.update-duckdns-ip = {
        serviceConfig.Type = "oneshot";
        script = ''
          ${
            if cfg.token != null then
              ''
                url="https://www.duckdns.org/update?domains=${cfg.subdomain}&token=${cfg.token}&verbose=true&ip="
              ''
            else
              ''
                token="$(< ${cfg.tokenPath})"
                url="https://www.duckdns.org/update?domains=${cfg.subdomain}&token=$token&verbose=true&ip="
              ''
          }
          echo url="$url" | ${pkgs.curl}/bin/curl -k -o /tmp/duck.log -K - >/dev/null 2>&1
        '';
      };

      timers.update-duckdns-ip = {
        wantedBy = [ "timers.target" ];
        partOf = [ "update-duckdns-ip.service" ];
        timerConfig.OnCalendar = cfg.onCalendar;
      };
    };
  };
}
