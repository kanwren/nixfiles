{ config, pkgs, ... }:

{
  time = {
    timeZone = "America/New_York";
    hardwareClockInLocalTime = false;
  };

  location.provider = "geoclue2";
  services.localtime.enable = true;
}
