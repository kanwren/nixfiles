{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium.overrideAttrs (old: {
      installPhase = (old.installPhase or "") + ''
        ln -s "$out"/bin/codium "$out"/bin/code
      '';
    });
    extensions = with pkgs.vscode-extensions; [
      haskell.haskell
    ];
  };
}
