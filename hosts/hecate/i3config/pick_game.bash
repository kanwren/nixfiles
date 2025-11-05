#! @runtimeShell@
# shellcheck shell=bash

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

main() {
	declare -r game="$(find ~/Games/games -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | rofi -dmenu -p " [game]")"
	declare -r game_path=~/Games/games/"$game"
	[ -x "${game_path}/play" ] && cd "${game_path}" && ./play
}

main
