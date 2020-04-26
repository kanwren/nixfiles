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
      enable = false;
    };

  };

  environment.systemPackages = with pkgs; [
    # vagrant
  ];

  # Enable qemu-based emulation of other platforms
  qemu-user = {
    aarch64 = true;
  };
}
