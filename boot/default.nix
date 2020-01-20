{ ... }:

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

# For legacy boot, set
#   loader.grub = {
#     enable = true;
#     version = 2;
#     device = "/dev/sda";
#   };
# or:
#   loader.grub = {
#     enable = true;
#     version = 2;
#     efiSupport = true;
#     efiInstallAsRemovable = true;
#     device = "nodev"
#   };

