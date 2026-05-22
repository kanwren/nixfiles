{
  flake.modules.nixos.graphics =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.feh
        pkgs.ghostscript
        pkgs.qpdf
        pkgs.mpv
        pkgs.libreoffice
      ];
    };
}
