{ pkgs
, lib
, custom
, ...
}:

with rec {
  # Scripts to be available globally
  # All scripts should be custom packages
  scripts = custom.lib.attrsets.recursiveDerivations custom.pkgs.scripts;

  from-inputs = [
    custom.pkgs.nix-utils
  ];

  baseSystemPackages = with pkgs; [
    # Nix stuff
    cachix
    patchelf
    nix-index
    nix-prefetch-git

    # Core utils
    binutils-unwrapped
    moreutils
    usbutils
    pciutils
    dnsutils
    zlib

    # CLI tools
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
    linuxPackages.perf
    ctags
    tldr
    entr
    pdsh
    jq
    # Alternatives to coreutils
    ripgrep
    fd
    exa
    fzf

    # System stuff
    brightnessctl
    mons

    # CLI math tools
    bc
    libqalculate
    qalculate-gtk # graphical calculator

    # Compression tools
    gnutar
    gzip
    bzip2
    zip
    unzip
    xz
    unrar
    unar

    # Filesystem tools
    ntfsprogs

    # Crypto stuff
    mkpasswd
    gnupg
    gpg-tui
    openssl

    # Terminal emulators and multiplexers
    kitty
    tmux

    # Media
    exiftool # EXIF data
    feh # image viewer
    imagemagick7 # image manipulation tools
    gimp # image editor
    zathura # PDF viewer
    qpdf # PDF manipulation tool
    vlc # multimedia viewer
    mpv # multimedia viewer
    (asunder.override { mp3Support = true; oggSupport = true; }) # CD ripping
    simplescreenrecorder # screen recording

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
      from-inputs
    ];
    homeBinInPath = true;
    shellInit = ''
      export PATH="$HOME/.cabal/bin''${PATH:+:''${PATH}}"
    '';
  };
}
