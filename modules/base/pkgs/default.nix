{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # editor
      neovim

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
      ripgrep
      fd
      sd
      exa
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
    ];

    variables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}

