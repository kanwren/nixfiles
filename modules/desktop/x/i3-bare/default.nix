{ pkgs, config, ... }:

{
  services.xserver = {
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = pkgs.substituteAll {
        src = ./i3config/i3config;
        spill_container_script = pkgs.substituteAll {
          src = ../i3/i3config/spill_container.sh;
          isExecutable = true;
          inherit (pkgs) runtimeShell jq;
          i3 = config.services.xserver.windowManager.i3.package;
        };
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
