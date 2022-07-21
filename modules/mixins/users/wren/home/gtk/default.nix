{ pkgs, ... }:

{
  gtk = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
    };
    theme = {
      name = "Catppuccin-dark";
      package = pkgs.catppuccin-gtk;
    };
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
  };
}

