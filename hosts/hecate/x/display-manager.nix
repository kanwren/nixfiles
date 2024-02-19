{ pkgs, ... }:

{
  services.xserver.displayManager = {
    lightdm = {
      enable = true;
      greeters.gtk = {
        enable = true;

        clock-format = "%I:%M %p";

        theme = {
          name = "Catppuccin-Mocha-Standard-Lavender-Dark";
          package = pkgs.catppuccin-gtk.override {
            accents = [ "lavender" ];
            variant = "mocha";
          };
        };

        iconTheme = {
          name = "Papirus-Adapta-Nokto";
          package = pkgs.papirus-icon-theme;
        };
      };

      background = ../../../desktop-backgrounds/dark-cat.png;
    };
  };
}
