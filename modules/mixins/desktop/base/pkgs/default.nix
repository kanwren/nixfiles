{ pkgs, lib, ... }:

{
  environment = {
    systemPackages = with pkgs;[
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
  };

  nixpkgs.config.permittedInsecurePackages = [
    # https://github.com/NixOS/nixpkgs/issues/273611
    (lib.throwIf
      (lib.versionOlder "1.5.3" pkgs.obsidian.version)
      "obsidian-${pkgs.obsidian.version} no longer requires exception for EOL electron-25.9.0"
      "electron-25.9.0")
  ];
}

