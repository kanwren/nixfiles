let
  commonPackages =
    pkgs: with pkgs; [
      bat
      bat-extras.core
      ncdu
      nettools
      bc
      binutils-unwrapped
      bzip2
      certigo
      certstrap
      comma
      coreutils
      curl
      diffutils
      dos2unix
      dnsutils
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
      gptfdisk
      gzip
      imagemagick
      hyperfine
      jq
      just
      libqalculate
      lz4
      man-pages
      mkpasswd
      moreutils
      neovim
      net-tools
      netcat
      nix-diff
      nix-index
      nix-tree
      nmap
      openssl
      parallel
      parted
      gocryptfs
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
      tree
      unar
      unzip
      watch
      wget
      whois
      trash-cli
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
          gocryptfs
        ]);
    };

  flake.modules.darwin.base =
    { pkgs, ... }:
    {
      environment.systemPackages = commonPackages pkgs;
    };
}
