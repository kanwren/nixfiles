{
  flake.modules.homeManager.graphics =
    { config, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      home.pointerCursor.enable = true;

      gtk = {
        enable = isLinux;
        gtk4.theme = config.gtk.theme;
      };

      qt = {
        enable = isLinux;
        platformTheme.name = "gtk3";
      };
    };
}
