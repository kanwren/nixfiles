{ nix-cron
}:

{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    (import ./home-assistant.nix { inherit nix-cron; })
  ];

  sops.defaultSopsFile = ../secrets/secrets.yaml;

  boot = {
    loader = {
      # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
      grub.enable = false;
      # Enables the generation of /boot/extlinux/extlinux.conf
      generic-extlinux-compatible.enable = true;
    };
    cleanTmpDir = true;
  };

  networking = {
    hostName = "homepi";
    wireless.enable = true;
    useDHCP = false;
    interfaces = {
      eth0.useDHCP = true;
      wlan0 = {
        useDHCP = false;
        ipv4.addresses = [
          { address = "192.168.1.123"; prefixLength = 24; }
        ];
      };
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [ 631 ];
      allowedUDPPorts = [ 631 ];
    };
    nameservers = [
      "8.8.8.8"
    ];
    defaultGateway = "192.168.1.1";
  };

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users = {
    mutableUsers = true;
    users.rarer = {
      isNormalUser = true;
      extraGroups = [ "wheel" config.users.groups.keys.name ];
      initialPassword = "setup";
      uid = 1000;
    };
  };

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    gc = {
      automatic = true;
      options = "--delete-older-than 10d";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      vim
      libraspberrypi
      git
      gnupg
    ];
    variables = {
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
  };

  documentation.enable = false;

  system.stateVersion = "20.09";

}

