{ pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [ "nvidia_drm.fbdev=1" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };

    tmp.cleanOnBoot = true;

    # enable aarch64-linux emulation
    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };
}
