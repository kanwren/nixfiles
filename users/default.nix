{ pkgs, lib, ... }:

{
  users = {
    mutableUsers = true;
    users = {

      nprin = {
        name = "nprin";
        initialPassword = "pass";
        isNormalUser = true;
        uid = 1000;
        extraGroups = [
          "wheel"
          "audio"
          "video"
          "networkmanager"
          "docker"
          "vboxusers"
        ];
        createHome = true;
        home = "/home/nprin";
        shell = pkgs.bashInteractive;
      };

    };
  };
}

# Note: to generate an initialHashedPassword, use
# nix-shell -p mkpasswd --run 'mkpasswd -m sha-512 -s'
