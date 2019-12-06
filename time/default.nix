{ config, pkgs, ... }:

{
  time = {
    timeZone = "America/New_York";
    hardwareClockInLocalTime = false;
  };
}
