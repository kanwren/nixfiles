{ pkgs, lib, ... }:

{
  users.users.wren = {
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
      "dialout"
    ];
    createHome = true;
    shell = pkgs.zsh;

    openssh.authorizedKeys.keys = import ../../../../../keys/nprindle.nix;
  };

  nix.settings.trusted-users = [ "wren" ];
}

# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'
