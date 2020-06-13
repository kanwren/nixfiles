{ pkgs, config, ... }:

{
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";

    inputMethod = {
      enabled = "ibus";
      ibus = {
        engines = with pkgs.ibus-engines; [
          libpinyin
          kkc
        ];
      };
    };
  };

  systemd.user.services.ibus-daemon = {
    description = "ibus-daemon";

    wantedBy = [ "default.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];

    serviceConfig = {
      ExecStart = ''
        ${config.i18n.inputMethod.package}/bin/ibus-daemon
      '';
      Restart = "on-failure";
      RestartSec = 3;
      RestartPreventExitStatus = 3;
    };
  };
}
