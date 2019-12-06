{ config, pkgs, ... }:

{
  home-manager.users.nprin = {

    # programs.alacritty = {
    #   enable = true;
    # };

    home.packages = [ pkgs.alacritty ];

    home.file = {
      ".config/alacritty/alacritty.yml".source = ./alacritty.yml;
    };

  };
}
