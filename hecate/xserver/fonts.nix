{ pkgs, ... }:

{
  fonts = {
    fonts = with pkgs; [
      fira-mono
      fira-code
      fira-code-symbols
    ];
  };
}
