{ runCommand
, catppuccin-k9s-src
}:

let
  src = catppuccin-k9s-src;
in

runCommand "catppuccin-k9s" { } ''
  mkdir -p "$out"
  cp ${src}/dist/*.yml "$out"
''

