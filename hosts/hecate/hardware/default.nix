{ pkgs, ... }:

{
  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [
        libva
      ];
    };

    acpilight.enable = true;

    trackpoint.enable = true;
  };
}
