{
  flake.modules.nixos.graphics =
    { pkgs, ... }:
    {
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages32 = [ pkgs.pkgsi686Linux.libva ];
      };
    };
}
