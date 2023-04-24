{ runCommand
, catppuccin-btop-src
}:

let
  src = catppuccin-btop-src;
in

runCommand "catppuccin-btop.theme" { } ''
  mkdir -p "$out"/share/btop/themes
  cp ${src}/themes/*.theme "$out"/share/btop/themes
''

