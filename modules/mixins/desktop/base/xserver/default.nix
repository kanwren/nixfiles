{ pkgs, config, ... }:

{
  imports = [
    ./fonts.nix
    ./logind.nix
  ];

  services.xserver = {
    enable = true;

    # Enable touchpad
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
    };

    layout = "us";
    xkbOptions = "caps:swapescape,compose:rctrl";

    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;

          clock-format = "%I:%M %p";

          theme = {
            name = "Materia-dark";
            package = pkgs.materia-theme;
          };

          iconTheme = {
            name = "Papirus-Adapta-Nokto";
            package = pkgs.papirus-icon-theme;
          };
        };

        background = ../../../../../desktop-backgrounds/earth.png;
      };
    };
  };
}
