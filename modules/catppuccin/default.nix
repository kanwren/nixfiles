{ inputs, ... }:

{
  flake.modules.nixos.catppuccin =
    { pkgs, ... }:
    {
      services.xserver.displayManager.lightdm = {
        greeters.gtk = {
          theme = {
            name = "catppuccin-mocha-lavender-standard";
            package = pkgs.catppuccin-gtk.override {
              accents = [ "lavender" ];
              variant = "mocha";
            };
          };

          iconTheme = {
            name = "FairyWren_Dark";
            package = pkgs.fairywren;
          };
        };

        background = ../../desktop-backgrounds/hearts.png;
      };

      environment.systemPackages = [
        pkgs.fairywren
      ];
    };

  flake.modules.homeManager.catppuccin =
    {
      pkgs,
      config,
      ...
    }:
    {
      imports = [ inputs.catppuccin.homeModules.catppuccin ];

      catppuccin = {
        enable = true;
        autoEnable = true;
        flavor = "mocha";
        accent = "lavender";

        firefox.enable = false;
        kvantum.enable = false;
        mpv.enable = false;
        rofi.enable = false;
        yazi.enable = false;
        gtk.icon.enable = false;
      };

      xdg.enable = true;

      gtk = {
        enable = true;

        gtk4.theme = config.gtk.theme;

        theme = {
          name = "catppuccin-mocha-lavender-standard";
          package = pkgs.catppuccin-gtk.override {
            accents = [ "lavender" ];
            variant = "mocha";
          };
        };

        iconTheme =
          if pkgs.stdenv.hostPlatform.isLinux then
            {
              name = "FairyWren_Dark";
              package = pkgs.fairywren;
            }
          else
            null;
      };

      qt = {
        enable = pkgs.stdenv.hostPlatform.isLinux;
        platformTheme.name = "gtk";
      };
    };
}
