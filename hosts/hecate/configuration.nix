{
  pkgs,
  config,
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
    docker = {
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

    keybase.enable = true;

    pueue.enable = true;

    localtimed.enable = false;

    xserver = {
      enable = true;

      xkb = {
        layout = "us";
        options = "caps:escape";
      };

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
        background = ../../desktop-backgrounds/dark-cat.png;
      };

      windowManager.i3 = {
        enable = true;
        package = pkgs.i3;
        configFile = pkgs.replaceVars ./i3config/i3config {
          spill_container_script = pkgs.replaceVarsWith {
            src = ./i3config/spill_container.bash;
            isExecutable = true;
            replacements = {
              inherit (pkgs) runtimeShell jq;
              i3 = config.services.xserver.windowManager.i3.package;
            };
          };

          pick_game_script = pkgs.replaceVarsWith {
            src = ./i3config/pick_game.bash;
            isExecutable = true;
            replacements = {
              inherit (pkgs) runtimeShell rofi;
            };
          };

          copyq_launch_script = pkgs.replaceVarsWith {
            src = ./i3config/copyq_launch.bash;
            isExecutable = true;
            replacements = {
              inherit (pkgs) runtimeShell copyq;
            };
          };

          i3status_config = pkgs.writeText "i3status.conf" ''
            general {
                    colors = true
                    color_good = "#a6e3a1"
                    color_degraded = "#f9e2af"
                    color_bad = "#f38ba8"
                    interval = 5
            }

            order += "wireless _first_"
            order += "ethernet _first_"
            order += "battery all"
            order += "memory"
            order += "disk /"
            order += "tztime local"

            wireless _first_ {
                    format_up = "W %quality at %essid"
                    format_down = "W down"
            }

            ethernet _first_ {
                    format_up = "E %speed"
                    format_down = "E down"
            }

            battery all {
                    format = "%status %percentage %remaining"
            }

            memory {
                    format = "%used/%available"
                    threshold_degraded = "1G"
                    format_degraded = "MEM < %available"
            }

            disk "/" {
                    format = "%avail"
            }

            tztime local {
                    format = "%Y-%m-%d %H:%M:%S"
            }
          '';
        };
        extraPackages = with pkgs; [
          rofi
          i3status
          i3lock
          betterlockscreen
          copyq
          networkmanagerapplet
          blueman
          mate.mate-media
          playerctl
          alsa-utils
        ];
      };
    };

    picom = {
      enable = true;
      vSync = true;
      backend = "glx";
      settings = {
        blur = {
          method = "gaussian";
          size = 10;
          deviation = 5.0;
        };
      };
      shadow = true;
      fade = true;
      fadeDelta = 4;
    };

    redshift = {
      enable = true;
      executable = "/bin/redshift-gtk";
      brightness = {
        day = "1";
        night = "0.8";
      };
      temperature = {
        day = 6500;
        night = 3200;
      };
    };

    logind.settings.Login = {
      HandleLidSwitch = "suspend";
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
        xy = "xsel -ib";
        xp = "xsel -ob";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    cachix
    patchelf
    nix-index
    nix-prefetch-git
    nixpkgs-fmt
    sqlite-interactive
    binutils-unwrapped
    moreutils
    usbutils
    pciutils
    dnsutils
    findutils
    findutils.locate
    zlib
    parallel
    parted
    ntfsprogs
    bat
    bat-extras.core
    ripgrep
    fd
    sd
    eza
    fzf
    wget
    curl
    sshfs
    git
    whois
    perf
    hyperfine
    man-pages
    tldr
    cht-sh
    tree
    file
    dos2unix
    xxd
    entr
    ctags
    gnutar
    gzip
    bzip2
    zip
    unzip
    xz
    unrar
    unar
    bc
    libqalculate
    openssl
    mkpasswd
    gnupg
    gocryptfs
    cryptor
    kitty
    brightnessctl
    mons
    xsel
    exiftool
    feh
    imagemagick
    ffmpeg
    ghostscript
    gimp
    zathura
    qpdf
    mpv
    simplescreenrecorder
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

  home-manager.users.wren = {
    home = {
      stateVersion = "25.05";

      sessionPath = [ "$HOME/bin" ];

      sessionVariables = {
        EDITOR = "nvim";
        VISUAL = "nvim";
      };

      packages = with pkgs; [
        h
        tokei
        miniserve
        qrencode
        xfce.thunar
        bitwarden-desktop
        kdePackages.filelight
        signal-desktop
        obsidian
        libreoffice
        musescore
        qalculate-gtk
        anki
      ];
    };

    mixins = {
      enable = true;
      browsers.chromium.enable = false;
    };

    programs.jujutsu.settings = {
      user.email = "nicole@wren.systems";
      signing = {
        behavior = "own";
        backend = "gpg";
        key = "002937658A2F43138C3B267E339C3A5C672CEA46";
      };
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  system.stateVersion = "25.05";
}
