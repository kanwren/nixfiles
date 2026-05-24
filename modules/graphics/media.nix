{
  flake.modules.nixos.graphics =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.calibre
        pkgs.feh
        pkgs.ghostscript
        pkgs.libreoffice
        pkgs.mpv
        pkgs.qpdf
        pkgs.xournalpp
      ];
    };
}
