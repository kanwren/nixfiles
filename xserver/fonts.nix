{ pkgs, ... }:

{
  fonts = {
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
    ];
  };
}
