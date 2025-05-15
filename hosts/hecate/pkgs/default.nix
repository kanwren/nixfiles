{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # nix stuff
      cachix
      patchelf
      nix-index
      nix-prefetch-git
      nixpkgs-fmt
      sqlite-interactive

      # core utils
      binutils-unwrapped
      moreutils
      usbutils
      pciutils
      dnsutils
      findutils
      findutils.locate
      zlib
      parallel
      # filesystem tools
      parted
      ntfsprogs

      # CLI tools
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
      # profiling
      linuxPackages.perf
      hyperfine
      # help, docs
      man-pages
      tldr
      cht-sh
      # navigation, files, formats, data processing
      tree
      file
      dos2unix
      xxd
      entr # run commands when files change
      ctags # source code location indexing
      # compression
      gnutar
      gzip
      bzip2
      zip
      unzip
      xz
      unrar
      unar # compression multitool
      # math
      bc
      libqalculate

      # cryptography stuff
      openssl
      mkpasswd
      gnupg
      gocryptfs
      cryptor

      # desktop applications
      kitty # terminal

      # system management and other tools
      brightnessctl
      mons
      xsel # X clipboard access

      # media
      exiftool # EXIF data
      feh # image viewer
      imagemagick # image manipulation tools
      ffmpeg # audio/video manipulation
      ghostscript # to support imagemagick
      gimp # image editor
      zathura # PDF viewer
      qpdf # PDF manipulation tool
      mpv # multimedia viewer
      simplescreenrecorder # screen recording
    ];
  };
}

