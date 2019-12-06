{ config, pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = [ pkgs.zathura ];

    home.file = {
      ".config/zathura/.zathurarc".text = ''
        set statusbar-h-padding 0
        set statusbar-v-padding 0
        set page-padding 1
        map i recolor
        map p print
      '';
    };

  };
}

