{ pkgs, ... }:

{
  gtk = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
    };
    theme = {
      name = "Catppuccin-Mocha-Standard-Lavender-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "lavender" ];
        variant = "mocha";
      };
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

