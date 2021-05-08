{ pkgs, lib
, npkgs, nlib
, ...
}:

with rec {
  # Scripts to be available globally
  # All scripts should be custom packages
  scripts = nlib.attrsets.recursiveDerivations npkgs.scripts;

  baseSystemPackages = with pkgs; [
    # Nix stuff
    cachix
    patchelf
    nix-index

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
    jq
    unar
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
    units
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
    exiftool              # EXIF data
    feh                   # image viewer
    scrot                 # screenshot tool
    imagemagick7          # image manipulation tools
    gimp                  # image editor
    zathura               # PDF viewer
    qpdf                  # PDF manipulation tool
    vlc                   # multimedia viewer
    mpv                   # multimedia viewer
    (asunder.override {   # CD ripping
      mp3Support = true;
      oggSupport = true;
    })
    simplescreenrecorder  # screen recording

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
    homeBinInPath = true;
    shellInit = ''
      export PATH="$HOME/.cabal/bin:$PATH"
    '';
  };

}
