{ pkgs, lib, ... }:

{
  environment = {
    systemPackages = with pkgs; [
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
      findutils
      findutils.locate
      zlib
      # filesystem tools
      parted
      ntfsprogs

      # CLI tools
      bat
      ripgrep
      fd
      sd
      eza
      fzf
      wget
      curl
      sshfs
      git
      tmux # terminal multiplexer
      whois
      linuxPackages.perf # profiling
      # help, docs
      man-pages
      tldr
      cht-sh
      # navigation, files, formats, data processing
      tree
      file
      dos2unix
      entr # run commands when files change
      ctags # source code location indexing
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

      # math
      bc
      libqalculate

      # cryptography stuff
      openssl
      mkpasswd
      gnupg

      # desktop applications
      kitty # terminal

      # system management and other tools
      brightnessctl
      mons
      xclip # X clipboard access

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

      # apps
      firefox
      obsidian
      qalculate-gtk # graphical calculator
      plover.dev # steno
    ];

    variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };

  nixpkgs.config.permittedInsecurePackages = [
    # https://github.com/NixOS/nixpkgs/issues/273611
    (lib.throwIf
      (lib.versionOlder "1.5.3" pkgs.obsidian.version)
      "obsidian-${pkgs.obsidian.version} no longer requires exception for EOL electron-25.9.0"
      "electron-25.9.0")
  ];
}

