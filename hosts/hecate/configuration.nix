{
  pkgs,
  lib,
  ...
}:

{
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
    trusted-users = [
      "root"
      "wren"
    ];
    substituters = [ "https://cache.lix.systems" ];
    trusted-public-keys = [ "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o=" ];
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [ "nvidia_drm.fbdev=1" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 3;
    };

    tmp.cleanOnBoot = true;

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
      trustedInterfaces = [ "tailscale0" ];
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
    };
  };

  security = {
    sudo = {
      enable = true;
      package = pkgs.sudo;
      wheelNeedsPassword = true;
    };

    rtkit.enable = true;
  };

  virtualisation = {
    podman = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };
  };

  documentation = {
    # Cache generation takes a really long time when fish is enabled
    man.generateCaches = false;
  };

  services = {
    tailscale = {
      enable = true;
      useRoutingFeatures = "both";
      extraUpFlags = [
        "--ssh"
        "--advertise-exit-node"
      ];
    };

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

    blueman.enable = true;

    printing = {
      enable = true;
      openFirewall = true;
    };

    gnome.gnome-keyring.enable = true;

    atd.enable = true;

    pueue.enable = true;

    localtimed.enable = false;

    xserver = {
      enable = true;
      displayManager.lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;
          clock-format = "%I:%M %p";
          theme = {
            name = "catppuccin-mocha-lavender-standard";
            package = pkgs.catppuccin-gtk.override {
              accents = [ "lavender" ];
              variant = "mocha";
            };
          };
          iconTheme = {
            name = "Papirus-Adapta-Nokto";
            package = pkgs.papirus-icon-theme;
          };
        };
        background = ../../desktop-backgrounds/hearts.png;
      };
    };

    logind.settings.Login = {
      HandleLidSwitch = "ignore";
      HandleLidSwitchDocked = "ignore";
      HandleLidSwitchExternalPower = "ignore";
      HandlePowerKey = "ignore";
      IdleAction = "ignore";
    };
  };

  fonts.packages = [
    pkgs.nerd-fonts.fira-mono
    pkgs.nerd-fonts.fira-code
    pkgs.fira-code
    pkgs.fira-mono
    pkgs.source-code-pro
    pkgs.atkinson-hyperlegible
    pkgs.inter
  ];

  programs = {
    nix-ld.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-curses;
    };

    nm-applet.enable = true;

    fuse.userAllowOther = true;

    command-not-found.enable = false;

    fish = {
      enable = true;
      shellAbbrs = {
        xy = "wl-copy";
        xp = "wl-paste";
      };
    };

    steam.enable = true;

    niri.enable = true;
  };

  environment.systemPackages = [
    pkgs.bat
    pkgs.bat-extras.core
    pkgs.bc
    pkgs.binutils-unwrapped
    pkgs.bzip2
    pkgs.curl
    pkgs.dnsutils
    pkgs.dos2unix
    pkgs.exiftool
    pkgs.nettools
    pkgs.eza
    pkgs.fd
    pkgs.feh
    pkgs.ffmpeg
    pkgs.file
    pkgs.findutils
    pkgs.fzf
    pkgs.ghostscript
    pkgs.git
    pkgs.gnupg
    pkgs.gnutar
    pkgs.gocryptfs
    pkgs.gzip
    pkgs.imagemagick
    pkgs.libqalculate
    pkgs.man-pages
    pkgs.mkpasswd
    pkgs.mons
    pkgs.ncdu
    pkgs.nix-index
    pkgs.openssl
    pkgs.parallel
    pkgs.parted
    pkgs.patchelf
    pkgs.qpdf
    pkgs.ripgrep
    pkgs.swaylock
    pkgs.tldr
    pkgs.tree
    pkgs.unar
    pkgs.unzip
    pkgs.wget
    pkgs.whois
    pkgs.wl-clipboard
    pkgs.xxd
    pkgs.xz
    pkgs.zip
    pkgs.zlib
  ];

  users = {
    mutableUsers = true;
    users.wren = {
      initialPassword = "setup";
      isNormalUser = true;
      uid = 1000;
      extraGroups = [
        "wheel"
        "audio"
        "video"
        "networkmanager"
        "docker"
        "vboxusers"
        "libvirtd"
        "wireshark"
        "dialout"
      ];
      createHome = true;
      shell = pkgs.fish;
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.wren = {
      home = {
        stateVersion = "25.05";

        sessionPath = [ "$HOME/bin" ];

        sessionVariables = {
          EDITOR = "nvim";
          VISUAL = "nvim";
        };

        packages = [
          pkgs.bitwarden-desktop
          pkgs.libreoffice
          pkgs.mpv
          pkgs.obsidian
          pkgs.qalculate-gtk
          pkgs.signal-desktop
          pkgs.dolphin-emu
          pkgs.mcrcon
          pkgs.joycond-cemuhook
          pkgs.xwayland-satellite
        ];
      };

      mixins = {
        bash.enable = true;
        btop.enable = true;
        catppuccin.enable = true;
        direnv.enable = true;
        discord.enable = true;
        fish.enable = true;
        gh.enable = true;
        git.enable = true;
        gpg-agent.enable = true;
        h.enable = true;
        jq.enable = true;
        jujutsu.enable = true;
        nix.enable = true;
        zathura.enable = true;
        zellij.enable = true;
        zoxide.enable = true;
      };

      services.grobi = {
        enable = true;
        rules = [
          {
            name = "Dock";
            outputs_connected = [ "HDMI-1" ];
            configure_single = "HDMI-1";
            primary = "HDMI-1";
            atomic = true;
            execute_after = [
              "test -f ~/.fehbg && source ~/.fehbg"
            ];
          }
          {
            name = "Mobile";
            outputs_disconnected = [ "HDMI-1" ];
            configure_single = "eDP-1";
            primary = "eDP-1";
            atomic = true;
            execute_after = [
              "test -f ~/.fehbg && source ~/.fehbg"
            ];
          }
        ];
      };

      programs = {
        jujutsu.settings = {
          user.email = "nicole@wren.systems";
          signing = {
            behavior = "own";
            backend = "gpg";
            key = "002937658A2F43138C3B267E339C3A5C672CEA46";
          };
        };

        noctalia-shell = {
          enable = true;
          settings = {
            # TODO
          };
        };

        firefox.enable = true;

        ghostty = {
          enable = true;
          settings = {
            font-size = 11.0;
            font-family = "FiraCode Nerd Font Mono";
          };
        };
      };
    };
  };

  system.stateVersion = "25.05";
}
