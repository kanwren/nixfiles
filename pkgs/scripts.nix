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

  # Shortcut script for managing the redshift systemd service
  rsctlScript = pkgs.writeShellScriptBin "rsctl" ''
    case "$1" in
      start|stop|restart|status)
        arg="$1"
        ;;
      *)
        >&2 "Usage: rsctl <start|stop|restart|status>"
        exit 1
        ;;
    esac

    systemctl --user "$arg" redshift
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

  # Manually set the screen brightness using xrandr
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

  # Simple shortcut for projecting to a single HDMI output
  hdmiScript = with pkgs; writeShellScriptBin "hdmi" ''
    set -e

    if [ $# -ne 1 ]; then
      >&2 echo "Error: wrong number of arguments"
      >&2 echo "Usage: hdmi <on|left|right|up|above|down|below|mirror|off>"
      exit 1
    fi

    primary="$(xrandr | grep primary | head -n 1 | cut -d' ' -f 1)"

    if [ -z "$primary" ]; then
      >&2 echo "Error: no primary display found"
      exit 1
    else
      echo "Using $primary as primary display"
    fi

    hdmi="$(xrandr | grep HDMI | head -n 1 | cut -d' ' -f 1)"

    if [ -z "$hdmi" ]; then
      >&2 echo "Error: no HDMI display found"
      exit 1
    else
      echo "Using $hdmi as HDMI display"
    fi

    case "$1" in
      on)         xrandr --output $hdmi --auto              ;;
      left)       xrandr --output $hdmi --left-of  $primary ;;
      right)      xrandr --output $hdmi --right-of $primary ;;
      up|above)   xrandr --output $hdmi --above    $primary ;;
      down|below) xrandr --output $hdmi --below    $primary ;;
      mirror)     xrandr --output $hdmi --same-as  $primary ;;
      off)        xrandr --output $hdmi --off               ;;
      *)
        >&2 echo "Unrecognized argument: $arg"
        >&2 echo "Usage: hdmi <on|left|right|up|above|down|below|mirror|off>"
        ;;
    esac
  '';

  ghcWithPackagesScript = with pkgs; writeShellScriptBin "gwp" ''
    case "$1" in
      ghc*)
        ghc="$1"
        shift
        args="$*"
        ;;
      *)
        ghc="ghc865"
        args="$*"
        ;;
    esac
    ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithPackages (p: with p; [ $args ])"
  '';

  # Start a hoogle server, usually in a nix-shell
  hoogleServerScript = with pkgs; writeShellScriptBin "hoogleserver" ''
    if [ -n "$1" ]; then
      port="$1"
    else
      port="8080"
    fi
    hoogle server --port=8080 --local --haskell
  '';

in [
  extractScript
  bakScript
  rsctlScript
  nightScript
  brightScript
  hdmiScript
  ghcWithPackagesScript
  hoogleServerScript
]
