{ pkgs, ... }:

{
  home.packages = [ pkgs.libnotify pkgs.dunst ];

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    settings = {
      global = {
        geometry = "500x5-10+10";
        transparency = 10;
        frame_color = "#000000";
        font = "Fira Mono";
        corner_radius = 3;
        word_wrap = true;
        follow = "mouse";
        notification_height = 20;
        padding = 5;
        horizontal_padding = 5;
        icon_position = "right";
      };
      urgency_low = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
      urgency_critical = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
    };
  };
}

