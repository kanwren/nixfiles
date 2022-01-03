{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./home-assistant.nix
    ./tailscale.nix
  ];

  sops = {
    defaultSopsFile = ./secrets/secrets.yaml;
    gnupg = {
      home = "/var/lib/sops";
      sshKeyPaths = [ ];
    };
  };

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
    wireless = {
      enable = true;
      interfaces = [ "wlan0" ];
    };
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
    firewall.enable = true;
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
    allowedUsers = [ "@wheel" ];
  };

  environment = {
    defaultPackages = lib.mkForce [ ];
    systemPackages = with pkgs; [
      vim
    ];
    variables = {
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "prohibit-password";
      passwordAuthentication = false;
      allowSFTP = false;
      challengeResponseAuthentication = false;
      extraConfig = ''
        AllowTcpForwarding yes
        X11Forwarding no
        AllowAgentForwarding no
        AllowStreamLocalForwarding no
        AuthenticationMethods publickey
      '';
    };
  };

  security = {
    sudo.execWheelOnly = true;
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "tty";
    };
  };

  documentation.enable = false;

  system.stateVersion = "21.05";
}

