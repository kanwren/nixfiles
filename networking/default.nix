{ lib, ... }:

let
  readLines = path: builtins.filter (x: x != "") (lib.splitString "\n" (builtins.readFile path));
in
{
  networking = {

    hostName = "nprin";

    networkmanager = {
      enable = true;
    };

    wireless = {
      enable = false;
    };

    # Global useDHCP is deprecated
    useDHCP = false;
    # Obtain list of interfaces at /sys/class/net/*
    interfaces = {
      eno1.useDHCP = true;
      wlo1.useDHCP = true;
    };

    firewall = {
      enable = true;
      allowedTCPPorts = [ 631 ];
      allowedUDPPorts = [ 631 ];
    };

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

  };
}
