{ pkgs }:

let
  extractScript = with pkgs; writeShellScriptBin "extract" ''
    if [ -z "$1" ]; then
      echo "Usage: extract </path/to/file>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|sz|ex|tar.bz2|tar.gz|tar.xz>"
      else
      if [ -f $1 ]; then
        case $1 in
          *.tar.bz2)   ${gnutar} xvjf ./$1    ;;
          *.tar.gz)    ${gnutar} xvzf ./$1    ;;
          *.tar.xz)    ${gnutar} xvJf ./$1    ;;
          *.lzma)      ${xz} --format=lzma --decompress ./$1 ;;
          *.bz2)       ${bzip2}/bin/bzip2 -d ./$1 ;;
          *.rar)       ${unrar}/bin/unrar x -ad ./$1 ;;
          *.gz)        ${gzip}/bin/gzip -d ./$1      ;;
          *.tar)       ${gnutar} xvf ./$1     ;;
          *.tbz2)      ${gnutar} xvjf ./$1    ;;
          *.tgz)       ${gnutar} xvzf ./$1    ;;
          *.zip)       ${unzip}/bin/unzip ./$1       ;;
          *.Z)         ${ncompress}/bin/uncompress ./$1  ;;
          *.7z)        ${p7zip}/bin/7z x ./$1        ;;
          *.xz)        ${xz}/bin/xz --decompress ./$1 ;;
          *.exe)       ${cabextract}/bin/cabextract ./$1  ;;
          *.cab)       ${cabextract}/bin/cabextract ./$1  ;;
          *)           echo "extract: '$1' - unknown archive method" ;;
        esac
      else
        echo "$1 - file does not exist"
      fi
    fi
  '';

  bakScript = with pkgs; writeShellScriptBin "bak" ''
    if [ $# -eq 1 ]; then
      if [ -f "$1" ]; then
        cp "$1" "$1.bak"
        echo "Copied $1 to $1.bak"
      else
         >&2 echo "error: cannot find file $1"
      fi
    else
       >&2 echo "error: expected 1 argument, got $#"
    fi
  '';

  nightScript = pkgs.writeShellScriptBin "night" ''
    configure_night_mode() {
      for disp in $(xrandr | grep " connected" | cut -f1 -d " "); do
        echo "Configuring $disp..."
        xrandr --output $disp --gamma $1 --brightness $2
      done
    }

    case $1 in
      off) echo "Night mode off" && configure_night_mode 1:1:1 1.0 ;;
      *) echo "Night mode on" && configure_night_mode 1:1:0.5 0.7 ;;
    esac
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
        echo "Invalid brightness; enter a value between 0.2 and 1.0"
      fi
    }

    brightness "$1"
  '';

in [
  extractScript
  bakScript
  nightScript
  brightScript
]
