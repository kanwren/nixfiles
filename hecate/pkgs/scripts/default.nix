{ pkgs }:

let
  haskellScript = { name, contents, libraries ? (_: []) }: pkgs.writers.writeHaskellBin name {
    ghc = pkgs.haskell.packages.ghc8103.ghc;
    ghcArgs = [ "-O2" ];
    libraries = libraries (pkgs.haskell.packages.ghc8103);
  } (if builtins.isString contents then contents else builtins.readFile contents);

  scripts = {
    # simple random number generator script
    random = haskellScript {
      name = "random";
      libraries = p: with p; [ splitmix optparse-applicative ];
      contents = ./Random.hs;
    };

    # script for generating truth tables from boolean expressions
    truthtable = haskellScript {
      name = "truthtable" ;
      libraries = p: with p; [ megaparsec containers text ];
      contents = ./TruthTable.hs;
    };

    # convenience script to wrap 'docker run' for running CS2110 autograders
    autograde = with pkgs; runCommand "autograde-patch" {} ''
      install -D \
        "${writeShellScript "autograde" (builtins.readFile ./autograde.sh)}" \
        "$out"/bin/autograde
      substituteInPlace "$out"/bin/autograde \
        --subst-var-by docker "${docker}"
    '';

    # utility to uncorrupt CircuitSim files
    uncorrupt = with pkgs; runCommand "uncorrupt-patch" {} ''
      install -D \
        "${writeShellScript "uncorrupt" (builtins.readFile ./uncorrupt.sh)}" \
        "$out"/bin/uncorrupt
      substituteInPlace "$out"/bin/uncorrupt \
        --subst-var-by coreutils "${coreutils}" \
        --subst-var-by jq "${jq}"
    '';

    # Print nix garbage collector roots that still exist
    gcroots = with pkgs; writeShellScriptBin "nix-gcroots" ''
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

    bak = with pkgs; writeShellScriptBin "bak" ''
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

    nosleep = pkgs.writeShellScriptBin "nosleep" ''
      if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        >&2 echo "Usage: nosleep [seconds]"
        exit
      fi

      delay="$${1:-59}"

      while :; do
        ${pkgs.xdotool}/bin/xdotool mousemove_relative -- 1 0
        sleep "$delay"
        ${pkgs.xdotool}/bin/xdotool mousemove_relative -- -1 0
        sleep "$delay"
      done
    '';

    toggle = pkgs.writeShellScriptBin "toggle" ''
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

    ghcWithPackages = with pkgs; writeShellScriptBin "gwp" ''
      case "$1" in
        ghc*)
          ghc="$1"
          shift
          args="$*"
          ;;
        *)
          ghc="ghc8103"
          args="$*"
          ;;
      esac
      ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithPackages (p: with p; [ $args ])"
    '';

    ghcWithHoogle = with pkgs; writeShellScriptBin "gwh" ''
      case "$1" in
        ghc*)
          ghc="$1"
          shift
          args="$*"
          ;;
        *)
          ghc="ghc8103"
          args="$*"
          ;;
      esac
      ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithHoogle (p: with p; [ $args ])"
    '';

    # Start a hoogle server, usually in a nix-shell
    hoogleServer = with pkgs; writeShellScriptBin "hoogleserver" ''
      hoogle server --port=''${1:-8080} --local --haskell
    '';
  };
in builtins.attrValues scripts
