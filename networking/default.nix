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
    interfaces =
      let mkInterface = name: { inherit name; value = { useDHCP = true; }; };
      in lib.listToAttrs (builtins.map mkInterface (readLines ./interfaces.txt));

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

  };
}
