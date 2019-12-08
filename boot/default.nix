{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot = {

    cleanTmpDir = true;

    loader = {
      systemd-boot = {
        enable = true;
      };

      efi = {
        canTouchEfiVariables = true;
      };
    };

  };
}
