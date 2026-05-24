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
    { pkgs, ... }:
    {
      imports = [ inputs.catppuccin.homeModules.catppuccin ];

      catppuccin = {
        enable = true;
        flavor = "mocha";
        accent = "lavender";

        firefox.enable = false;
        kvantum.enable = false;
        mpv.enable = false;
        rofi.enable = false;
        yazi.enable = false;
      };

      xdg.enable = true;

      gtk = {
        enable = true;

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

      qt = {
        enable = true;
        platformTheme.name = "gtk";
      };
    };
}
