{ config, pkgs, ... }:

{
  # Enable sound.
  sound.enable = true;

  hardware = {
    pulseaudio = {
      enable = true;
      # Bluetooth is only available in the full build
      package = pkgs.pulseaudioFull;
    };

    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    brightnessctl.enable = true;

    trackpoint.enable = true;
  };
}
