{ nord-dircolors }:

{ pkgs, lib, ... }:

with rec {
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
    niv
    pkgs.nur.repos.xe.comma # TODO: remove this once it's in nixpkgs
    pkgs.nur.repos.xe.pridecat
    fortune

    # Core utils
    binutils-unwrapped
    moreutils
    usbutils
    pciutils
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
    # Alternatives to coreutils
    ripgrep
    fd
    exa
    fzf
    dust
    procs

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
    (import ./bash/default.nix { inherit nord-dircolors; })
    (import ./zsh/default.nix { inherit nord-dircolors; })
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
