{ pkgs, ... }:

{
  sound.enable = true;

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [
        libva
      ];
    };

    acpilight.enable = true;

    trackpoint.enable = true;

    pulseaudio = {
      enable = true;
      # Bluetooth is only available in the full build
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };

    bluetooth = {
      enable = true;
      powerOnBoot = true;
      package = pkgs.bluez;
    };
  };

  # Enable touchpad
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    mouse = {
      accelProfile = "adaptive";
      accelSpeed = "1.0";
    };
  };
}
