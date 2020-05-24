{ pkgs, ... }:

{
  imports = [
    ./qemu.nix
  ];

  virtualisation = {

    docker = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };

    virtualbox.host = {
      enable = true;
    };

  };

  environment.systemPackages = with pkgs; [
    vagrant
  ];

  # Enable qemu-based emulation of other platforms
  qemu-user = {
    aarch64 = true;
  };
}
