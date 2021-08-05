{ pkgs, ... }:

{
  home.packages = [ pkgs.kitty ];

  home.file = {
    ".config/kitty/kitty.conf".source = ./kitty.conf;
  };
}

