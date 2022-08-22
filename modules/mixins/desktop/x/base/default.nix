{ pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  services.xserver = {
    enable = true;

    # Enable touchpad
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    layout = "us";
    xkbOptions = "caps:swapescape"; # TODO: set good compose key

    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;

          clock-format = "%I:%M %p";

          theme = {
            name = "Catppuccin-Purple-Dark";
            package = pkgs.catppuccin-gtk;
          };

          iconTheme = {
            name = "Papirus-Adapta-Nokto";
            package = pkgs.papirus-icon-theme;
          };
        };

        background = ../../../../../desktop-backgrounds/dark-cat.png;
      };
    };
  };
}
