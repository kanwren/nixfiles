{ pkgs
, lib
, custom
, ...
}:

with rec {
  # Scripts to be available globally
  # All scripts should be custom packages
  scripts = custom.lib.attrsets.recursiveDerivations {
    inherit (custom.pkgs.scripts)
      lipsum
      nosleep
      toggle
      add-rpath;
  };

  from-inputs = [
    custom.pkgs.nix-utils
  ];

  baseSystemPackages = with pkgs; [
    # nix stuff
    cachix
    patchelf
    nix-index
    nix-prefetch-git

    # core utils
    binutils-unwrapped
    moreutils
    usbutils
    pciutils
    dnsutils
    zlib

    # alternatives to coreutils
    ripgrep
    fd
    exa
    fzf

    # file transfer, requests
    wget
    curl
    sshfs

    # version control
    git

    # help, docs
    manpages
    tldr
    cht-sh

    # stdenv compiler
    stdenv.cc
    stdenv.cc.cc.man

    # system management and utilities
    killall
    whois
    linuxPackages.perf # profiling
    xclip # X clipboard access
    brightnessctl
    mons

    # administrative tools
    pdsh # parallel remote shell

    # navigation, files, formats, data processing
    tree
    file
    dos2unix
    entr # run commands when files change
    ctags # source code browsing
    jq # JSON processor

    # compression
    gnutar
    gzip
    bzip2
    zip
    unzip
    xz
    unrar
    unar # compression multitool

    # filesystem stuff
    parted
    ntfsprogs

    # math
    bc
    libqalculate
    qalculate-gtk # graphical calculator

    # cryptography stuff
    openssl
    mkpasswd
    gnupg

    # terminal emulators and multiplexers
    kitty
    tmux

    # media
    exiftool # EXIF data
    feh # image viewer
    imagemagick7 # image manipulation tools
    gimp # image editor
    zathura # PDF viewer
    qpdf # PDF manipulation tool
    vlc # multimedia viewer
    mpv # multimedia viewer
    simplescreenrecorder # screen recording

    # browsers
    firefox

    # misc
    cudatoolkit
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
      from-inputs
    ];
    homeBinInPath = true;
  };
}
