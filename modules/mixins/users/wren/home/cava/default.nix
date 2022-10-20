{ pkgs, self, ... }:

{
  home.packages = [ pkgs.cava ];
  xdg.configFile."cava/config".text = ''
    ${builtins.readFile "${self.packages.${pkgs.system}.catppuccin-cava}/mocha.cava"}
  '';
}

