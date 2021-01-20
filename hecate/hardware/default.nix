{ pkgs, ... }:

{
  sound.enable = true;

  hardware = {
    pulseaudio = {
      enable = true;
      # Bluetooth is only available in the full build
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };

    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [
        libva
      ];
    };

    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    acpilight.enable = true;

    trackpoint.enable = true;
  };
}
