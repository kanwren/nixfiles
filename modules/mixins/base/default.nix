let
  modules = {
    boot = ./boot;
    i18n = ./i18n;
    nix = ./nix;
    pkgs = ./pkgs;
    security = ./security;
    services = ./services;
    shells = ./shells;
    time = ./time;
    users = ./users;
    networking = ./networking;
    starship = ./starship.nix;
  };
in
modules // {
  full = {
    imports = builtins.attrValues modules;
  };
}
