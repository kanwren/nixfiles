{ runCommand
, catppuccin-zathura-src
}:

let
  src = catppuccin-zathura-src;
in

runCommand "catppuccin-zathura" { } ''
  mkdir -p $out/share/zathura/themes
  cp ${src}/src/* $out/share/zathura/themes
''
