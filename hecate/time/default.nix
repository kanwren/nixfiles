{ ... }:

{
  time = {
    timeZone = "America/New_York";
    hardwareClockInLocalTime = true;
  };

  location.provider = "geoclue2";
  services.localtime.enable = true;
}
