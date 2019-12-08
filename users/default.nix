{ config, pkgs, ... }:

{
  users.users = {
    nprin = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [
        "wheel"
        "audio"
        "video"
        "networkmanager"
      ];

      createHome = true;
      home = "/home/nprin";
      shell = pkgs.bashInteractive;
    };
  };
}
