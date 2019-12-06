{ config, pkgs, ... }:

{
  # Relies on location.provider
  services.redshift = {
    enable = true;
    brightness = {
      day = "1";
      night = "0.6";
    };
    temperature = {
      day = 6500;
      night = 4200;
    };
  };
}
