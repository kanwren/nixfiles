{
  flake.modules.homeManager.zathura = {
    programs.zathura = {
      enable = true;
      options = {
        statusbar-h-padding = 0;
        statusbar-v-padding = 0;
        page-padding = 1;
        database = "sqlite";
      };
      extraConfig = ''
        map i recolor
        map p print
      '';
    };
  };
}
