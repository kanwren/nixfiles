{ pkgs, lib, ... }:

let
  utils = import ../utils { inherit lib; };
in with rec {
  # Scripts to be available globally
  scripts = import ./scripts { inherit pkgs; };

  baseSystemPackages = with pkgs; [
    # Nix stuff
    nix-universal-prefetch
    nix-prefetch-git
    nix-prefetch-github
    nix-prefetch-docker
    cachix
    patchelf
    nix-index

    # Core utils
    binutils-unwrapped
    usbutils
    pciutils

    # Basic CLI tools
    wget
    curl
    git
    killall
    manpages
    whois
    tree
    file
    dos2unix
    xclip
    ag
    tldr
    linuxPackages.perf
    entr
    ctags

    # System stuff
    brightnessctl
    mons

    # CLI math tools
    bc
    units
    libqalculate
    # graphical calculator
    qalculate-gtk

    # Compression tools
    gnutar
    gzip
    bzip2
    zip
    unzip
    xz
    unrar

    # Filesystem tools
    parted
    ntfsprogs

    # Crypto stuff
    mkpasswd
    gnupg
    openssl

    # Terminal emulators and multiplexers
    kitty
    tmux

    # Media
    # EXIF data
    exiftool
    # Images
    feh
    scrot
    imagemagick7
    gimp
    # PDFs
    zathura
    qpdf

    # Multimedia
    vlc
    # CD ripping
    (asunder.override {
      mp3Support = true;
      oggSupport = true;
    })
    # Recording
    simplescreenrecorder

    # Browsers
    firefox
  ];
};

{
  imports = [
    ./vim/default.nix
    ./bash/default.nix
    ./zsh/default.nix
  ];

  environment = {
    systemPackages = lib.concatLists [
      baseSystemPackages
      scripts
    ];
  };

}
