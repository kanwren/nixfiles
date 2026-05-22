let
  module =
    { pkgs, ... }:
    {
      fonts.packages = [
        pkgs.atkinson-hyperlegible
        pkgs.atkinson-hyperlegible-next # TODO: stable
        pkgs.fira-code
        pkgs.fira-mono
        pkgs.inter
        pkgs.nerd-fonts.fira-code
        pkgs.nerd-fonts.fira-mono
        pkgs.source-code-pro
      ];
    };
in
{
  flake.modules.nixos.graphics = module;
  flake.modules.darwin.graphics = module;
}
