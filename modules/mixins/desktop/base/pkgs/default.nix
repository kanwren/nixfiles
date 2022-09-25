{ pkgs, ... }:

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
      qalculate-gtk # graphical calculator
    ];
  };
}

