{ ... }:

{
  time = {
    timeZone = "America/New_York";
    hardwareClockInLocalTime = true;
  };

  location.provider = "geoclue2";
  services.localtimed.enable = true;
}
