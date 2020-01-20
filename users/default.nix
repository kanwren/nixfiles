{ pkgs, lib, ... }:

let
  utils = import ../utils { inherit lib; };
  secrets = utils.importOr ../secrets.nix {};
in
{
  users = {
    mutableUsers = false;
    users = {

      nprin = {
        name = "nprin";
        initialHashedPassword =
          let path = [ "users" "nprin" "hashedPassword" ];
          in lib.attrByPath path null secrets;
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
