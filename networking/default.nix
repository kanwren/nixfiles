{ lib, ... }:

let
  utils = import ../utils { inherit lib; };
  networks = utils.importOr ./networks.nix {};
  readLines = path: builtins.filter (x: x != "") (lib.splitString "\n" (builtins.readFile path));
in {
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

  # Add networkmanager configuration files for predefined networks
  environment.etc =
    let
      nmConn = name: "NetworkManager/system-connections/${name}.nmconnection";
      nmConfig = text: { inherit text; mode = "0400"; };
      # Map a network name/config text pair into a filename/submodule pair
      nmConnFile = { name, value }: {
        name = nmConn name;
        value = nmConfig value;
      };
    in utils.mapAttrPairs nmConnFile networks;
}
