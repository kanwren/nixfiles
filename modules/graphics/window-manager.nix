{ config, ... }:

{
  flake.modules = {
    nixos.graphics =
      { pkgs, ... }:
      {
        programs = {
          niri.enable = true;
          fish.shellAbbrs = {
            xy = "wl-copy";
            xp = "wl-paste";
          };
        };

        environment.systemPackages = [
          pkgs.wl-clipboard
          pkgs.swaylock
          pkgs.xwayland-satellite
        ];
      };

    darwin.graphics = {
      imports = [
        config.flake.modules.darwin.aerospace
      ];

      system.defaults = {
        dock = {
          autohide = true;
          orientation = "bottom";
          show-process-indicators = true;
          showhidden = true;
          mru-spaces = false;
          expose-group-apps = true;
        };

        finder = {
          AppleShowAllExtensions = true;
          AppleShowAllFiles = true;
          CreateDesktop = false;
          ShowStatusBar = true;
          ShowPathbar = true;
          FXEnableExtensionChangeWarning = false;
        };

        spaces = {
          spans-displays = true;
        };

        NSGlobalDomain = {
          NSWindowShouldDragOnGesture = true;
        };
      };

      programs.fish.shellAbbrs = {
        xy = "pbcopy";
        xp = "pbpaste";
      };
    };
  };
}
