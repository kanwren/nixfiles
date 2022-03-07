{ pkgs, lib, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      # editor
      # the full config is self.packages.${system{.neovim-with-plugin-deps,
      # which I usually put into the profile for the sake of flexibility
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
      zlib
      # filesystem tools
      parted
      ntfsprogs

      # CLI tools
      ripgrep
      fd
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

