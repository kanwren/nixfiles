{ config, pkgs, ... }:

{
  # Enable sound.
  sound.enable = true;

  hardware = {
    pulseaudio.enable = true;

    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    brightnessctl.enable = true;

    trackpoint.enable = true;
  };
}
