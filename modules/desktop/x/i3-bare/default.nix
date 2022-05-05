{ pkgs, config, ... }:

{
  services.xserver = {
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = pkgs.substituteAll {
        src = ./i3config/i3config;
      };

      extraPackages = with pkgs; [
        # Menu/launcher
        rofi
        # Status bar
        i3status
        # Lock screen
        i3lock
      ];
    };
  };
}
