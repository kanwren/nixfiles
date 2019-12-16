{ config, pkgs, ... }:

{
  boot = {

    cleanTmpDir = true;

    loader = {
      timeout = 3;

      # Use the systemd-boot EFI boot loader.
      systemd-boot = {
        enable = true;
      };

      efi = {
        canTouchEfiVariables = true;
      };
    };

  };
}
