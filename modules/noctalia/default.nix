{
  inputs,
  ...
}:

{
  flake.modules.homeManager.noctalia =
    { pkgs, ... }:
    {
      imports = [
        inputs.noctalia.homeModules.default
      ];

      programs.noctalia-shell = {
        enable = true;
        settings = {
          # TODO
        };
      };

      home.packages = [
        pkgs.adw-gtk3 # fixes GTK theming, per https://github.com/noctalia-dev/noctalia-shell/issues/1713
        pkgs.qt6Packages.qt6ct # fixes some QT theming, such as Dolphin
      ];
    };
}
