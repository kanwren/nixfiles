{ pkgs }:

let
  extractScript = pkgs.writeShellScriptBin "extract" ''
    if [ -z "$1" ]; then
    echo "Usage: extract </path/to/file>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|sz|ex|tar.bz2|tar.gz|tar.xz>"
    else
    if [ -f $1 ]; then
    case $1 in
      *.tar.bz2)   ${pkgs.gnutar} xvjf ./$1    ;;
      *.tar.gz)    ${pkgs.gnutar} xvzf ./$1    ;;
      *.tar.xz)    ${pkgs.gnutar} xvJf ./$1    ;;
      *.lzma)      ${pkgs.xz} --format=lzma --decompress ./$1 ;;
      *.bz2)       ${pkgs.bzip2}/bin/bzip2 -d ./$1 ;;
      *.rar)       ${pkgs.unrar}/bin/unrar x -ad ./$1 ;;
      *.gz)        ${pkgs.gzip}/bin/gzip -d ./$1      ;;
      *.tar)       ${pkgs.gnutar} xvf ./$1     ;;
      *.tbz2)      ${pkgs.gnutar} xvjf ./$1    ;;
      *.tgz)       ${pkgs.gnutar} xvzf ./$1    ;;
      *.zip)       ${pkgs.unzip}/bin/unzip ./$1       ;;
      *.Z)         ${pkgs.ncompress}/bin/uncompress ./$1  ;;
      *.7z)        ${pkgs.p7zip}/bin/7z x ./$1        ;;
      *.xz)        ${pkgs.xz}/bin/xz --decompress ./$1 ;;
      *.exe)       ${pkgs.cabextract}/bin/cabextract ./$1  ;;
      *.cab)       ${pkgs.cabextract}/bin/cabextract ./$1  ;;
      *)           echo "extract: '$1' - unknown archive method" ;;
    esac
    else
      echo "$1 - file does not exist"
    fi
    fi
  '';
in [
  extractScript
]
