{ ... }:

{
  location.provider = "geoclue2";
  services.localtime.enable = true;

  services.redshift = {
    enable = true;
    brightness = {
      day = "1";
      night = "0.5";
    };
    temperature = {
      day = 6500;
      night = 2700;
    };
  };
}
