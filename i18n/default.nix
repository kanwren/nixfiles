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
}
