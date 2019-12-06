{ config, pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = [ pkgs.alacritty ];

    home.file = {
      ".config/alacritty/alacritty.yml".source = ./alacritty.yml;
    };

  };
}
