{ pkgs, ... }:

{
  fonts = {
    packages = with pkgs; [
      (nerdfonts.override { fonts = [ "FiraMono" "FiraCode" ]; })
      fira-code
      fira-mono
      source-code-pro
      atkinson-hyperlegible
      inter
    ];
  };
}
