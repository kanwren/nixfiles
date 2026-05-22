{
  flake.modules.nixos.graphics =
    { pkgs, ... }:
    {
      services.xserver = {
        enable = true;
        displayManager.lightdm = {
          enable = true;
          greeters.gtk = {
            enable = true;
            clock-format = "%I:%M %p";
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
      };
    };
}
