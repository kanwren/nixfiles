{ pkgs, ... }:

{
  gtk = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
    };
    theme = {
      name = "Nordic";
      package = pkgs.nordic;
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

