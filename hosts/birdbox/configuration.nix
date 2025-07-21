{ config, lib, pkgs, ... }:

{
  imports = [
    ./disk-config.nix
    ./impermanence.nix
    ./services/llms.nix
    ./services/photos.nix
    ./tailscale.nix
    ./users/default.nix
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    tmp.cleanOnBoot = true;

    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };

  system = {
    autoUpgrade.enable = false;
  };

  sops.age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];

  nix = {
    settings = {
      keep-outputs = true;
      keep-derivations = true;
      trusted-users = [ "root" "wren" ];
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
  };

  networking = {
    networkmanager.enable = true;
    firewall.trustedInterfaces = [ config.services.tailscale.interfaceName ];
  };

  time = {
    timeZone = "America/Los_Angeles";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  security = {
    sudo = {
      extraConfig = ''
        Defaults lecture = never
      '';
    };
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    nix-ld = {
      enable = true;
    };

    fuse = {
      userAllowOther = true;
    };

    fish = {
      enable = true;
    };
  };

  services = {
    logind = {
      lidSwitch = "suspend";
      lidSwitchExternalPower = "ignore";
      lidSwitchDocked = "ignore";
      extraConfig = ''
        IdleAction=ignore
        HandlePowerKey=ignore
      '';
    };

    pipewire = {
      enable = true;
      pulse.enable = true;
    };

    libinput.enable = true;

    openssh = {
      enable = true;
      allowSFTP = true;
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    patchelf
    nix-index
    nixpkgs-fmt
    binutils-unwrapped
    moreutils
    usbutils
    pciutils
    dnsutils
    findutils
    findutils.locate
    cifs-utils
    zlib
    parallel
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
    man-pages
    tree
    file
    xxd
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
    neovim
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}

