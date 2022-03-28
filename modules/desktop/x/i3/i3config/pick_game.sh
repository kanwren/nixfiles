# Pick a game with rofi. This assumes the specific directory convention I like
# to use with games, where ~/Games/sources contains the game distributions, and
# ~/Games/games contains the unpacked game directories, each of which should
# have a "play" script, that might look something like this:
#
# ```
# #!/usr/bin/env nix-shell
# #!nix-shell -i bash -p steam-run-native
# cd $(dirname $0)
# nvidia-offload steam-run ./Celeste
# ```

PATH="@rofi@/bin${PATH:+:${PATH}}"

set -euo pipefail

game="$(ls "$HOME"/Games/games | rofi -dmenu -p " [game]")"
if [ "$?" -eq 0 ]; then
  cd "$HOME"/Games/games/"$game"
  ./play
fi

