{ pkgs, lib, ... }:

with rec {
  scripts = import ./scripts.nix { inherit pkgs; };

  baseSystemPackages = with pkgs; [
    # Nix stuff
    nix-universal-prefetch
    nix-prefetch-git
    cachix

    # Basic CLI tools
    binutils-unwrapped
    wget
    curl
    git
    killall
    manpages
    whois
    bc
    units
    tree
    xclip

    # Compression tools
    gnutar
    gzip
    bzip2
    unzip
    xz
    unrar
    ncompress
    p7zip
    cabextract

    # Filesystem tools
    parted
    gparted
    ntfsprogs

    # Crypto stuff
    mkpasswd
    gnupg
    paperkey
    openssl

    # Pentesting
    nmap-graphical
    hashcat

    # Terminal emulators and multiplexers
    kitty
    alacritty
    tmux

    # Media
    # EXIF data
    exiftool
    # Data matrices
    dmtx
    # Images
    feh
    scrot
    imagemagick7
    libwebp
    gifsicle
    gimp
    inkscape
    # PDFs
    zathura
    # Multimedia
    ffmpeg
    vlc
    # CD ripping
    (asunder.override {
      mp3Support = true;
      oggSupport = true;
    })

    # Browsers
    w3m-nographics
    firefox
  ];
};

{
  imports = [
    ./vim/default.nix
    ./bash/default.nix
  ];

  environment = {
    systemPackages = lib.concatLists [
      baseSystemPackages
      scripts
    ];
  };

}
