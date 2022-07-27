{ pkgs, ... }:

{
  xdg.configFile."nixpkgs/config.nix".source = ./config.nix;

  home.packages = with pkgs; [
    comma
    nix-tree
    nix-top
  ];
}
