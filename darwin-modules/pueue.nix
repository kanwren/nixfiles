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

    launchd.user.agents.pueue = {
      command = "${cfg.package}/bin/pueued --verbose";
      serviceConfig = {
        RunAtLoad = true;
        StandardOutPath = cfg.logFile;
        StandardErrorPath = cfg.logFile;
      };
    };
  };
}
