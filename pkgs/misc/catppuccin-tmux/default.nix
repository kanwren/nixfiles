{ runCommand
, catppuccin-tmux-src
}:

let
  src = catppuccin-tmux-src;
in

runCommand "catppuccin-tmux" { } ''
  mkdir -p "$out"
  cp -r "${src}"/* "$out"
''
