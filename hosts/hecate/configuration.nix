{
  pkgs,
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
        y = "waycopy";
        p = "waypaste";
      };
    };

    sway.enable = true;
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
    pkgs.kitty
    pkgs.libqalculate
    pkgs.man-pages
    pkgs.mkpasswd
    pkgs.nix-index
    pkgs.openssl
    pkgs.parallel
    pkgs.parted
    pkgs.patchelf
    pkgs.qpdf
    pkgs.ripgrep
    pkgs.tldr
    pkgs.tree
    pkgs.unar
    pkgs.unzip
    pkgs.wget
    pkgs.whois
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
    users.wren =
      { pkgs, ... }:
      {
        home = {
          stateVersion = "25.05";

          sessionPath = [ "$HOME/bin" ];

          sessionVariables = {
            EDITOR = "nvim";
            VISUAL = "nvim";
          };

          packages = [
            pkgs.bitwarden-desktop
            pkgs.kdePackages.filelight
            pkgs.libreoffice
            pkgs.mpv
            pkgs.obsidian
            pkgs.qalculate-gtk
            pkgs.signal-desktop
            pkgs.xfce.thunar
          ];
        };

        mixins = {
          bash.enable = true;
          btop.enable = true;
          browsers.firefox.enable = true;
          catppuccin.enable = true;
          cava.enable = true;
          direnv.enable = true;
          discord.enable = true;
          dunst.enable = true;
          fish.enable = true;
          flameshot.enable = true;
          gh.enable = true;
          git.enable = true;
          gpg-agent.enable = true;
          gtk.enable = true;
          h.enable = true;
          jq.enable = true;
          jujutsu.enable = true;
          kitty.enable = true;
          nix.enable = true;
          rofi.enable = true;
          rust.enable = true;
          spotify.enable = true;
          sway.enable = true;
          zathura.enable = true;
          zellij.enable = true;
          zoxide.enable = true;
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
  };

  system.stateVersion = "25.05";
}
