{ pkgs, ... }:
{
  imports = [
    ./caches.nix
    ./pkgs
    ./shells
    ./tailscale.nix
    ./users
    ./x
  ];

  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
    trusted-users = [ "root" ];
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [ "nvidia_drm.fbdev=1" ];
    kernelModules = [ "hid_nintendo" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };

    tmp.cleanOnBoot = true;

    # enable aarch64-linux emulation
    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };

  networking = {
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };

    firewall = {
      enable = true;
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ ];
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  # redshift is broken with geoclue2; use manual location provider in the meantime: https://github.com/jonls/redshift/issues/895
  location = {
    provider = "manual";
    latitude = 34.052235;
    longitude = -118.243683;
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";

    inputMethod = {
      enable = true;
      type = "ibus";
      ibus = {
        engines = with pkgs.ibus-engines; [
          libpinyin
        ];
      };
    };
  };

  hardware = {
    nvidia = {
      prime = {
        intelBusId = "PCI:5:0:0";
        nvidiaBusId = "PCI:1:0:0";
      };
      open = false;
    };

    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [
        libva
      ];
    };

    nvidia-container-toolkit.enable = true;

    acpilight.enable = true;

    trackpoint.enable = true;

    bluetooth = {
      enable = true;
      powerOnBoot = true;
      package = pkgs.bluez;
      input = {
        General = {
          ClassicBondedOnly = false;
          UserspaceHID = true;
        };
      };
    };
  };

  security = {
    sudo = {
      enable = true;
      package = pkgs.sudo;
      wheelNeedsPassword = true;
    };

    chromiumSuidSandbox.enable = false; # TODO

    rtkit.enable = true;
  };

  virtualisation = {
    docker = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };
  };

  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
      mouse = {
        accelProfile = "adaptive";
        accelSpeed = "1.0";
      };
    };

    openssh = {
      enable = true;
      allowSFTP = true;
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
        X11Forwarding = true;
      };
    };

    # bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    # Nintendo Pro controller
    joycond.enable = true;

    # cups
    printing = {
      enable = true;
      openFirewall = true;
    };

    gnome.gnome-keyring.enable = true;

    atd.enable = true;

    keybase.enable = true;

    pueue.enable = true;

    localtimed.enable = false;
  };

  programs = {
    nix-ld.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-curses;
    };

    nm-applet.enable = true;

    fuse.userAllowOther = true;
  };

  documentation = {
    # Cache generation takes a really long time when fish is enabled
    man.generateCaches = false;
  };

  system.stateVersion = "25.05";
}
