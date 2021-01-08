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

    nvidia.prime = {
      offload.enable = true;
      intelBusId = "PCI:1:0:0";
      nvidiaBusId = "PCI:1:0:0";
    };

    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    trackpoint.enable = true;
  };
}
