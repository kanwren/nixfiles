{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.ollama;
in {
  options = {
    services.ollama.enable = mkOption {
      type = types.bool;
      default = false;
      description = lib.mdDoc "Whether to enable the ollama service.";
    };

    services.ollama.package = mkOption {
      type = types.path;
      default = pkgs.ollama;
      defaultText = "pkgs.ollama";
      description = lib.mdDoc "The ollama package to use.";
    };

    services.ollama.logFile = mkOption {
      type = types.path;
      default = null;
      example = "/var/tmp/ollama.log";
      description = lib.mdDoc "The logfile to use for ollama";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [cfg.package];

    launchd.user.agents.ollama = {
      command = "${cfg.package}/bin/ollama serve";
      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        StandardOutPath = cfg.logFile;
        StandardErrorPath = cfg.logFile;
      };
    };
  };
}
