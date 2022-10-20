{ runCommand
, catppuccin-kitty-src
}:

let
  src = catppuccin-kitty-src;
in

runCommand "catppuccin-kitty" { } ''
  mkdir -p "$out"
  cp ${src}/*.conf "$out"
''

