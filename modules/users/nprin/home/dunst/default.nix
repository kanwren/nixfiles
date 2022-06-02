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
        # transparency = 20;
        frame_color = "#96CDFB";
        separator_color = "frame";
        frame_width = 2;
        word_wrap = true;
        follow = "mouse";
        font = "FiraCode Nerd Font Mono";
        markup = "full";
        format = "%s\\n%b";
      };
      urgency_low = {
        background = "#1E1E2E";
        foreground = "#D9E0EE";
        timeout = 10;
      };
      urgency_normal = {
        background = "#1E1E2E";
        foreground = "#D9E0EE";
        timeout = 10;
      };
      urgency_critical = {
        background = "#1E1E2E";
        foreground = "#D9E0EE";
        frame_color = "#F8BD96";
        timeout = 10;
      };
    };
  };
}

