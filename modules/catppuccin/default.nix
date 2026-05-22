{ inputs, ... }:

{
  flake.modules.homeManager.catppuccin =
    { pkgs, config, ... }:
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

        gtk4.theme = config.gtk.theme;
      };

      qt = {
        enable = true;
        platformTheme.name = "gtk";
      };
    };
}
