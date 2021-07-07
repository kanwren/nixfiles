{ pkgs, ... }:

{
  boot = {
    # TODO[NixOS/nixpkgs/issues/129233]: switch to `linuxPackages_latest` once resolved
    kernelPackages = pkgs.linuxPackages_5_12;

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
