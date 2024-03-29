{ self }:

{ pkgs, ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      statusbar-h-padding = 0;
      statusbar-v-padding = 0;
      page-padding = 1;
    };
    extraConfig = ''
      map i recolor
      map p print
      include ${self.packages.${pkgs.system}.catppuccin-zathura}/share/zathura/themes/catppuccin-mocha
    '';
  };
}

