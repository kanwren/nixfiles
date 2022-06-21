{ pkgs, self, system, ... }:

{
  home.packages = [ pkgs.cava ];
  xdg.configFile."cava/config".text = ''
    ${builtins.readFile "${self.packages.${system}.catppuccin-cava}/catppuccin.cava"}
  '';
}

