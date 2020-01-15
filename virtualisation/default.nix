{ config, pkgs, ... }:

{
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
      # enableExtensionPack = true;
    };

  };
}
