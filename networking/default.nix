{ config, pkgs, ... }:

let
  importOr = path: default:
    if builtins.pathExists path then import path else default;
  secrets = importOr ../secrets.nix {};
in
{
  networking = {
    hostName = "nprin";

    networkmanager = {
      enable = true;
    };

    useDHCP = false;
    interfaces.eno1.useDHCP = true;
    interfaces.wlo1.useDHCP = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };
}
