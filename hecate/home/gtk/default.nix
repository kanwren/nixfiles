{ pkgs, ... }:

{
  gtk = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
    };
    theme = {
      name = "Materia-dark";
      package = pkgs.materia-theme;
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

