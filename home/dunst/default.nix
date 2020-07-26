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
        padding = 5;
        horizontal_padding = 5;
        notification_height = 20;
        corner_radius = 3;
        icon_position = "right";
        max_icon_size = 75;
        transparency = 20;
        frame_color = "#88c0d0";
        frame_width = 2;
        word_wrap = true;
        follow = "mouse";
        font = "Fira Mono";
        markup = "full";
        format = "%s\n%b";
      };
      urgency_low = {
        background = "#000000";
        foreground = "#ffffff";
        timeout = 10;
      };
      urgency_normal = {
        background = "#000000";
        foreground = "#ffffff";
        timeout = 10;
      };
      urgency_critical = {
        background = "#000000";
        foreground = "#ffffff";
        timeout = 10;
      };
    };
  };
}

