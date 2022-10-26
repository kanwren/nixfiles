{ self }:

{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;

    extraConfig = ''
      ${builtins.readFile ./kitty.conf}

      include ${self.packages.${pkgs.system}.catppuccin-kitty}/mocha.conf
    '';
  };
}

