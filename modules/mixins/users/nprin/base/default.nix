{ pkgs, lib, ... }:

{
  users.users.nprin = {
    initialPassword = "setup";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "audio"
      "video"
      "networkmanager"
      "docker"
      "vboxusers"
      "libvirtd"
      "wireshark"
    ];
    createHome = true;
    shell = pkgs.zsh;
  };

  nix.trustedUsers = [ "nprin" ];
}

# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'