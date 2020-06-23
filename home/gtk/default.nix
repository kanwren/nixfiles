{ pkgs, ... }:

{
  gtk = {
    enable = true;
    font = {
      name = "Fira Mono";
      package = pkgs.fira-mono;
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

