{ self }:

{ ... }:

{
  imports = [ self.inputs.catppuccin.homeManagerModules.catppuccin ];

  catppuccin.flavor = "mocha";

  xdg.enable = true;

  programs.kitty.catppuccin.enable = true;
  programs.btop.catppuccin.enable = true;
  programs.cava.catppuccin.enable = true;
  programs.tmux.catppuccin.enable = true;
  programs.zathura.catppuccin.enable = true;
}
