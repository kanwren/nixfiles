{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.services.pueue;
in
{
  options = {
    services.pueue.enable = mkOption {
      type = types.bool;
      default = false;
      description = lib.mdDoc "Whether to enable the pueue service.";
    };

    services.pueue.package = mkOption {
      type = types.path;
      default = pkgs.pueue;
      defaultText = "pkgs.pueue";
      description = lib.mdDoc "The pueue package to use.";
    };

    services.pueue.logFile = mkOption {
      type = types.path;
      default = null;
      example = "/var/tmp/pueue.log";
      description = lib.mdDoc "The logfile to use for pueue";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    systemd.user.services.pueue = {
      description = "Pueue task manager";
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        ExecStart = "${cfg.package}/bin/pueued --verbose";
      };
      wantedBy = [ "default.target" ];
    };
  };
}
