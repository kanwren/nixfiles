{ self }:

let
  modules = {
    boot = ./boot;
    i18n = ./i18n;
    nix = ./nix;
    pkgs = ./pkgs;
    security = ./security;
    services = ./services;
    shells = import ./shells { inherit self; };
    time = ./time;
    users = ./users;
    networking = ./networking;
  };
in
modules // {
  full = {
    imports = builtins.attrValues modules;
  };
}
