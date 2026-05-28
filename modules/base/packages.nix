let
  commonPackages =
    pkgs: with pkgs; [
      bat
      bat-extras.core
      bc
      binutils-unwrapped
      bzip2
      certigo
      certstrap
      comma
      coreutils
      curl
      diffutils
      dnsutils
      dos2unix
      exiftool
      eza
      fd
      ffmpeg
      file
      findutils
      fzf
      gawk
      gitFull
      gnugrep
      gnumake
      gnused
      gnutar
      gocryptfs
      gptfdisk
      gzip
      hyperfine
      imagemagick
      jq
      just
      libqalculate
      libwebp
      lz4
      man-pages
      mkpasswd
      moreutils
      ncdu
      neovim
      net-tools
      netcat
      nettools
      nix-diff
      nix-index
      nix-tree
      nmap
      openssl
      parallel
      patch
      patchelf
      ripgrep
      rsync
      screen
      sd
      socat
      sqlite-interactive
      sshfs
      tealdeer
      tmux
      trash-cli
      tree
      unar
      unzip
      watch
      wget
      whois
      xxd
      xz
      yq-go
      zip
      zstd
    ];
in
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      environment.systemPackages =
        commonPackages pkgs
        ++ (with pkgs; [
          unixtools.util-linux
          parted
          gocryptfs
        ]);
    };

  flake.modules.darwin.base =
    { pkgs, ... }:
    {
      environment.systemPackages = commonPackages pkgs;
    };
}
