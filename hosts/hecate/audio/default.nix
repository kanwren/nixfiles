{ pkgs, ... }:

{
  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    # Bluetooth is only available in the full build
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };
}

