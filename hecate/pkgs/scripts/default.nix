{ pkgs }:

let
  scripts = {
    # simple random number generator script
    randomScript = pkgs.haskell.packages.ghc8102.callPackage ./random {};

    # Print nix garbage collector roots that still exist
    gcrootsScript = with pkgs; writeShellScriptBin "nix-gcroots" ''
      echo "/nix/var/nix/gcroots/auto:"
      for f in /nix/var/nix/gcroots/auto/*; do
        if [ -e "$f" ]; then
          link="$(readlink "$f")"
          echo "    $link"
        fi
      done

      for d in /nix/var/nix/gcroots/per-user/*; do
        echo "$d:"
        for f in $d/*; do
          if [ -e "$f" ]; then
            link="$(readlink "$f")"
            echo "    $link"
          fi
        done
      done
    '';

    lorriGcScript = pkgs.haskell.packages.ghc8102.callPackage ./lorri-gc {};

    direnvAllowedScript = with pkgs; writeShellScriptBin "direnv-allowed" ''
      cat ~/.local/share/direnv/allow/*
    '';

    bakScript = with pkgs; writeShellScriptBin "bak" ''
      if [ $# -ge 1 ]; then
        if [ "$1" = "-u" ]; then
          shift
          for arg in "$@"; do
            if [ -f "$arg" ]; then
              case "$arg" in
                *.bak)
                  mv "$arg" "''${arg%.*}"
                  echo "Moved $arg to ''${arg%.*}"
                  ;;
                *)
                  >&2 echo "Error: $arg is not a .bak file"
                  ;;
              esac
            else
               >&2 echo "Error: cannot find file $arg"
            fi
          done
        else
          for arg in "$@"; do
            if [ -f "$arg" ]; then
              cp "$arg" "$arg.bak"
              echo "Copied $arg to $arg.bak"
            else
               >&2 echo "Error: cannot find file $arg"
            fi
          done
        fi
      else
         >&2 echo "Usage: bak [-u] <file1> [ <file2>...]"
         exit 1
      fi
    '';

    toggleScript = pkgs.writeShellScriptBin "toggle" ''
      if [ $# -ne 1 ]; then
        >&2 echo "Usage: toggle PROGRAM"
        exit 1
      fi
      progname="$1"
      status="$(systemctl --user is-active "$progname")"
      case "$status" in
        active)
          systemctl --user stop "$progname"
          echo "$progname has been toggled off"
          ;;
        inactive)
          systemctl --user restart "$progname"
          echo "$progname has been toggled on"
          ;;
        *)
          >&2 echo "error: status is $status"
          exit 1
          ;;
      esac
    '';

    # Manually enable/disable a night mode by adjusting the gamma/brightness with
    # xrandr
    nightScript = pkgs.writeShellScriptBin "night" ''
      configure_night_mode() {
        for disp in $(xrandr | grep " connected" | cut -f1 -d " "); do
          echo "Configuring $disp..."
          xrandr --output $disp --gamma $1 --brightness $2
        done
      }

      if [ "$1" = "off" ]; then
        echo "Night mode off"
        configure_night_mode 1:1:1 1.0
      elif [[ "$1" =~ 0\.[2-9][0-9]*|1(\.0+)? ]]; then
        echo "Night mode on"
        configure_night_mode 1:1:0.5 "$1"
      elif [ -z "$1" ]; then
        echo "Night mode on"
        configure_night_mode 1:1:0.5 0.7
      else
        >&2 echo "Usage: night [<off|0.2-1.0>]"
        exit 1
      fi
    '';

    ghcWithPackagesScript = with pkgs; writeShellScriptBin "gwp" ''
      case "$1" in
        ghc*)
          ghc="$1"
          shift
          args="$*"
          ;;
        *)
          ghc="ghc8102"
          args="$*"
          ;;
      esac
      ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithPackages (p: with p; [ $args ])"
    '';

    ghcWithHoogleScript = with pkgs; writeShellScriptBin "gwh" ''
      case "$1" in
        ghc*)
          ghc="$1"
          shift
          args="$*"
          ;;
        *)
          ghc="ghc8102"
          args="$*"
          ;;
      esac
      ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithHoogle (p: with p; [ $args ])"
    '';

    # Start a hoogle server, usually in a nix-shell
    hoogleServerScript = with pkgs; writeShellScriptBin "hoogleserver" ''
      hoogle server --port=''${1:-8080} --local --haskell
    '';
  };
in builtins.attrValues scripts
