{ runCommand
, catppuccin-tmux-src
}:

let
  src = catppuccin-tmux-src;
in

runCommand "catppuccin-tmux" { } ''
  mkdir -p "$out"
  cp "${src}"/*.tmux "${src}"/*.tmuxtheme "$out"
''
