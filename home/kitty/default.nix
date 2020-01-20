{ pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = [ pkgs.kitty ];

    home.file = {
      ".config/kitty/kitty.conf".source = ./kitty.conf;
    };

  };
}

