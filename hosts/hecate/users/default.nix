{ pkgs, lib, ... }:

{
  users = {
    mutableUsers = true;
    users = {
      nprin = {
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
        ];
        createHome = true;
        shell = pkgs.zsh;
      };
    };
  };
}

# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'
