{ pkgs, ... }:

{
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";

    inputMethod = {
      enable = true;
      type = "ibus";
      ibus = {
        engines = with pkgs.ibus-engines; [
          libpinyin
          # TODO: build failing
          # kkc
          table
          table-chinese
        ];
      };
    };
  };
}
