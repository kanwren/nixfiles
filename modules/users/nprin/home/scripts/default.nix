{ pkgs, lib, config, system, self, inputs, ... }:

let
  scripts = {
    # Pick a game with rofi. This assumes the specific directory convention I
    # like to use with games, where ~/Games/sources contains the game
    # distributions, and ~/Games/games contains the unpacked game directories,
    # each of which should have a "play" script, that might look something like
    # this:
    #
    # ```
    # #!/usr/bin/env nix-shell
    # #!nix-shell -i bash -p steam-run-native
    # cd $(dirname $0)
    # nvidia-offload steam-run ./Celeste
    # ```
    pick-game = pkgs.writeShellScriptBin "pick-game" ''
      set -euo pipefail
      game="$(ls "$HOME"/Games/games | ${pkgs.rofi}/bin/rofi -dmenu -p "Game")"
      if [ "$?" -eq 0 ]; then
        cd "$HOME"/Games/games/"$game"
        ./play
      fi
    '';
  };
in
{
  home.packages = builtins.attrValues scripts;
}

