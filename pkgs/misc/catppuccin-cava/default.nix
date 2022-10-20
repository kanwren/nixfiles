{ runCommand
, catppuccin-cava-src
}:

let
  src = catppuccin-cava-src;
in

runCommand "catppuccin-cava" { } ''
  mkdir -p "$out"
  cp -r "${src}"/* "$out"
''
