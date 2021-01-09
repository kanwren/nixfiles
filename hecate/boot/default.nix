{ pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };
    cleanTmpDir = true;

    # enable aarch64-linux emulation
    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };
}
