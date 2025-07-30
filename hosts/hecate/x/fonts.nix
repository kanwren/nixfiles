{pkgs, ...}: {
  fonts = {
    packages = with pkgs; [
      nerd-fonts.fira-mono
      nerd-fonts.fira-code
      fira-code
      fira-mono
      source-code-pro
      atkinson-hyperlegible
      inter
    ];
  };
}
