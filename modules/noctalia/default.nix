{
  inputs,
  ...
}:

{
  flake.modules.nixos.noctalia = {
    imports = [
      inputs.noctalia-greeter.nixosModules.default
    ];

    programs.noctalia-greeter = {
      enable = true;
      settings = {
        idle.timeout = 300;
      };
    };

    services.power-profiles-daemon.enable = true;
  };

  flake.modules.homeManager.noctalia =
    { pkgs, ... }:
    {
      imports = [
        inputs.noctalia.homeModules.default
      ];

      programs.noctalia = {
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
