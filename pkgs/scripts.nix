{ pkgs }:

let
  extractScript = with pkgs; writeShellScriptBin "extract" ''
    if [ -z "$1" ]; then
      >&2 echo "Usage: extract <filepath>.<tar.bz2|tar.gz|tar.xz|lzma|bz2|rar|gz|tar|tbz2|tgz|zip|Z|7z|xz|exe|cab>"
      exit 1
    else
      for arg in "$@"; do
        if [ -f "$arg" ]; then
          case "$arg" in
            *.tar.bz2)   ${gnutar}/bin/tar xvjf "$arg" ;;
            *.tar.gz)    ${gnutar}/bin/tar xvzf "$arg" ;;
            *.tar.xz)    ${gnutar}/bin/tar xvJf "$arg" ;;
            *.lzma)      ${xz}/bin/lzma --format=lzma --decompress "$arg" ;;
            *.bz2)       ${bzip2}/bin/bzip2 -d "$arg" ;;
            *.rar)       ${unrar}/bin/unrar x -ad "$arg" ;;
            *.gz)        ${gzip}/bin/gzip -d "$arg" ;;
            *.tar)       ${gnutar}/bin/tar xvf "$arg" ;;
            *.tbz2)      ${gnutar}/bin/tar xvjf "$arg" ;;
            *.tgz)       ${gnutar}/bin/tar xvzf "$arg" ;;
            *.zip)       ${unzip}/bin/unzip "$arg" ;;
            *.Z)         ${ncompress}/bin/uncompress "$arg" ;;
            *.7z)        ${p7zip}/bin/7z x "$arg" ;;
            *.xz)        ${xz}/bin/xz --decompress "$arg" ;;
            *.exe)       ${cabextract}/bin/cabextract "$arg" ;;
            *.cab)       ${cabextract}/bin/cabextract "$arg" ;;
            *)
              >&2 echo "extract: '$arg' - unknown archive method"
              exit 1
              ;;
          esac
        else
          >&2 echo "$arg - file does not exist"
          exit 1
        fi
      done
    fi
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

  # xrandr --output $(xrandr | grep " connected" | cut -f1 -d " ") --brightness 1
  brightScript = pkgs.writeShellScriptBin "bright" ''
    brightness() {
      if [[ -z $1 ]]; then
        brightness 1
      elif [[ "$1" =~ 0\.[2-9][0-9]*|1(\.0+)? ]]; then
        echo "Brightness: $1"
        for disp in $(xrandr | grep " connected" | cut -f1 -d " "); do
          echo "Setting $disp..."
          xrandr --output $disp --brightness $1
        done
      else
        >&2 echo "Invalid brightness; enter a value between 0.2 and 1.0"
        exit 1
      fi
    }

    brightness "$1"
  '';

  ghcWithPackagesScript = with pkgs; writeShellScriptBin "gwp" ''
    case "$1" in
      ghc*)
        args=''${*:2}
        ghc="$1"
        ;;
      *)
        args="$*"
        ghc="ghc865"
        ;;
    esac
    ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithPackages (p: with p; [ $args ])"
  '';

in [
  extractScript
  bakScript
  nightScript
  brightScript
  ghcWithPackagesScript
]
