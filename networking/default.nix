{ config, pkgs, lib, ... }:

let
  utils = import ../utils { inherit lib; };
  secrets = utils.importOr ../secrets.nix {};
  networks = lib.attrByPath [ "networks" "nmconns" ] {} secrets;
in {
  networking = {

    hostName = "nprin";

    networkmanager = {
      enable = true;
    };

    wireless = {
      enable = false;
    };

    useDHCP = false;
    interfaces.eno1.useDHCP = true;
    interfaces.wlo1.useDHCP = true;

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
