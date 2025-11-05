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
        configFile = pkgs.writeText "i3config" ''
          # keys
          set $mod Mod4
          floating_modifier $mod

          # display
          font pango:DejaVu Sans Mono 12
          for_window [class=".*"] border pixel 1
          hide_edge_borders both
          gaps inner 10
          gaps outer 10

          # startup programs
          exec --no-startup-id compton
          exec --no-startup-id copyq
          exec --no-startup-id nm-applet
          exec --no-startup-id blueman-applet
          exec --no-startup-id mate-volume-control-status-icon
          exec --no-startup-id ibus-daemon --daemonize --replace
          exec --no-startup-id flameshot

          # basic keybindings
          bindsym $mod+Shift+q kill
          bindsym $mod+i exec i3-input -f pango:monospace 12
          bindsym $mod+Shift+i exec betterlockscreen -l dimblur
          bindsym $mod+Shift+o exec betterlockscreen -s dimblur
          bindsym $mod+Shift+Return exec "rofi -modi drun,run,ssh -show drun -fuzzy"
          bindsym $mod+Shift+w exec "rofi -show window"
          bindsym Print exec flameshot gui
          bindsym $mod+Return exec kitty
          bindsym $mod+b exec firefox
          bindsym $mod+n exec copyq show

          # media buttons
          bindsym XF86AudioRaiseVolume exec --no-startup-id amixer sset Master 5%+ # Increase sound volume
          bindsym XF86AudioLowerVolume exec --no-startup-id amixer sset Master 5%- # Decrease sound volume
          bindsym XF86AudioMute        exec --no-startup-id amixer sset Master 1+ toggle # Toggle mute
          bindsym XF86MonBrightnessDown exec xbacklight -dec 5
          bindsym XF86MonBrightnessUp   exec xbacklight -inc 5
          bindsym XF86AudioPlay  exec playerctl play-pause
          bindsym XF86AudioPause exec playerctl pause
          bindsym XF86AudioNext  exec playerctl next
          bindsym XF86AudioPrev  exec playerctl previous
          bindsym XF86Calculator exec qalculate-gtk
          bindsym --release XF86PowerOff mode "system"

          # system commands
          bindsym $mod+c mode "quickcommand"
          mode "quickcommand" {
              bindsym r               restart;                mode "default"
              bindsym Shift+e         exit;                   mode "default"
              bindsym Control+Shift+s exec shutdown now;      mode "default"
              bindsym Control+Shift+r exec shutdown -r now;   mode "default"
              bindsym Escape mode "default"
          }

          # tiling movement
          bindsym $mod+h focus left
          bindsym $mod+j focus down
          bindsym $mod+k focus up
          bindsym $mod+l focus right
          bindsym $mod+Shift+h move left
          bindsym $mod+Shift+j move down
          bindsym $mod+Shift+k move up
          bindsym $mod+Shift+l move right
          bindsym $mod+Control+Shift+h move left 80 px
          bindsym $mod+Control+Shift+j move down 80 px
          bindsym $mod+Control+Shift+k move up 80 px
          bindsym $mod+Control+Shift+l move right 80 px

          # window layouts and properties
          bindsym $mod+Shift+minus split v
          bindsym $mod+Shift+backslash split h
          bindsym $mod+s layout stacking
          bindsym $mod+w layout tabbed
          bindsym $mod+e layout toggle split
          bindsym $mod+p focus parent
          bindsym $mod+Shift+p focus child
          bindsym $mod+Shift+space floating toggle
          bindsym --whole-window $mod+button2 floating toggle
          bindsym $mod+space focus mode_toggle
          bindsym $mod+f fullscreen toggle
          bindsym $mod+y sticky toggle
          bindsym $mod+Shift+BackSpace move scratchpad
          bindsym $mod+backslash scratchpad show

          # float certain windows automatically
          for_window [class="^Thunar$"] floating enable
          for_window [class="^Qalculate-gtk$"] floating enable
          for_window [class="^copyq$"] floating enable

          # resizing
          bindsym $mod+r mode "resize"
          mode "resize" {
              bindsym h resize shrink width 10 px or 1 ppt
              bindsym j resize grow height 10 px or 1 ppt
              bindsym k resize shrink height 10 px or 1 ppt
              bindsym l resize grow width 10 px or 1 ppt

              bindsym Shift+h resize shrink width 50 px or 5 ppt
              bindsym Shift+j resize grow height 50 px or 5 ppt
              bindsym Shift+k resize shrink height 50 px or 5 ppt
              bindsym Shift+l resize grow width 50 px or 5 ppt

              bindsym Control+Shift+h resize shrink width 150 px or 15 ppt
              bindsym Control+Shift+j resize grow height 150 px or 15 ppt
              bindsym Control+Shift+k resize shrink height 150 px or 15 ppt
              bindsym Control+Shift+l resize grow width 150 px or 15 ppt

              bindsym Escape mode "default"
          }

          # marking
          bindsym $mod+m exec i3-input -F 'mark --add %s' -l 1 -P 'mark: '
          bindsym $mod+Shift+m exec i3-input -F 'unmark %s' -l 1 -P 'mark: '
          bindsym $mod+Control+Shift+m unmark
          bindsym $mod+g exec i3-input -F '[con_mark="%s"] focus' -l 1 -P 'goto: '

          # automatic marks
          for_window [class="^discord$"] mark d
          for_window [class="^Spotify$"] mark s
          for_window [class="^Bitwarden$"] mark p

          # swapping
          bindsym $mod+bracketleft mark --add swapee
          bindsym $mod+bracketright swap container with mark swapee; unmark swapee

          # bar control
          bindsym $mod+u bar mode toggle
          bindsym $mod+Shift+u bar mode invisible
          bindsym $mod+Control+Shift+u bar hidden_state toggle

          # workspaces
          set $ws1  "1"
          set $ws2  "2"
          set $ws3  "3"
          set $ws4  "4"
          set $ws5  "5"
          set $ws6  "6"
          set $ws7  "7"
          set $ws8  "8"
          set $ws9  "9"
          set $ws10 "10"

          bindsym $mod+1 workspace $ws1
          bindsym $mod+2 workspace $ws2
          bindsym $mod+3 workspace $ws3
          bindsym $mod+4 workspace $ws4
          bindsym $mod+5 workspace $ws5
          bindsym $mod+6 workspace $ws6
          bindsym $mod+7 workspace $ws7
          bindsym $mod+8 workspace $ws8
          bindsym $mod+9 workspace $ws9
          bindsym $mod+0 workspace $ws10

          bindsym $mod+Left  workspace prev
          bindsym $mod+Right workspace next
          bindsym $mod+Tab   workspace back_and_forth

          bindsym $mod+Shift+1 move container to workspace $ws1
          bindsym $mod+Shift+2 move container to workspace $ws2
          bindsym $mod+Shift+3 move container to workspace $ws3
          bindsym $mod+Shift+4 move container to workspace $ws4
          bindsym $mod+Shift+5 move container to workspace $ws5
          bindsym $mod+Shift+6 move container to workspace $ws6
          bindsym $mod+Shift+7 move container to workspace $ws7
          bindsym $mod+Shift+8 move container to workspace $ws8
          bindsym $mod+Shift+9 move container to workspace $ws9
          bindsym $mod+Shift+0 move container to workspace $ws10

          bindsym $mod+Control+h move workspace to output left
          bindsym $mod+Control+j move workspace to output down
          bindsym $mod+Control+k move workspace to output up
          bindsym $mod+Control+l move workspace to output right

          # i3status
          set $base #1e1e2e
          set $text #cdd6f4
          set $pink #f5c2e7
          set $lavender #b4befe
          set $blue #89b4fa
          set $red #f38ba8
          bar {
              status_command i3status -c ${
                pkgs.writeText "i3status.conf" ''
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
                ''
              }
              hidden_state hide
              mode dock
              modifier $mod
              colors {
                  background         #131020
                  statusline         $text
                  focused_statusline $text
                  separator          $text
                  focused_separator  $text
                  focused_workspace  $base $pink $base
                  active_workspace   $base $base $blue
                  inactive_workspace $base $base $text
                  urgent_workspace   $base $base $red
                  binding_mode       $base $lavender $base
              }
          }

          # desktop background
          exec sh ~/.fehbg
        '';
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
    pkgs.mons
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
    pkgs.xsel
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
          pkgs.kdePackages.filelight
          pkgs.libreoffice
          pkgs.mpv
          pkgs.obsidian
          pkgs.qalculate-gtk
          pkgs.signal-desktop
          pkgs.xfce.thunar
          pkgs.zathura
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
  };

  system.stateVersion = "25.05";
}
