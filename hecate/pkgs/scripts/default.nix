{ pkgs }:

let
  haskellScript = { name, contents, libraries ? (_: []) }: pkgs.writers.writeHaskellBin name {
    ghc = pkgs.haskell.packages.ghc8104.ghc;
    ghcArgs = [ "-O2" "-Wall" "-Werror" ];
    libraries = libraries (pkgs.haskell.packages.ghc8104);
  } (if builtins.isString contents then contents else builtins.readFile contents);

  scripts = {
    # serve directories/files over HTTP
    serve = haskellScript {
      name = "serve";
      libraries = p: with p; [ warp wai-extra wai-middleware-static lucid optparse-applicative ];
      contents = ./Serve.hs;
    };

    # simple random number generator script
    random = haskellScript {
      name = "random";
      libraries = p: with p; [ splitmix optparse-applicative ];
      contents = ./Random.hs;
    };

    # script for generating truth tables from boolean expressions
    truthtable = haskellScript {
      name = "truthtable" ;
      libraries = p: with p; [ megaparsec haskeline containers ];
      contents = ./TruthTable.hs;
    };

    # convenience script to wrap 'docker run' for running CS2110 autograders
    autograde = pkgs.substituteAll {
      name = "autograde";
      src = ./autograde.sh;
      inherit (pkgs) docker;
      dir = "bin";
      isExecutable = true;
    };

    # utility to uncorrupt CircuitSim files
    uncorrupt = pkgs.substituteAll {
      name = "uncorrupt";
      src = ./uncorrupt.sh;
      inherit (pkgs) coreutils jq;
      dir = "bin";
      isExecutable = true;
    };

    # utility to view the revision history of a CircuitSim file
    csrh = pkgs.writeShellScriptBin "csrh" ''
      set -euo pipefail

      define() { IFS=$'\n' read -r -d ''' "''${1}" || true; }
      description="View CircuitSim revision history"
      usage_text=""
      define usage_text <<'EOF'
      USAGE:
          csrh <FILE>

      OPTIONS:
          -h, --help
                  Show this help text

      ARGS:
          <FILE>
                  CircuitSim file to view
      EOF

      print_help() { >&2 echo -e "$description\n\n$usage_text"; }
      print_usage() { >&2 echo "$usage_text"; }

      if ! args=$(getopt -o h --long help -n csrh -- "$@"); then
        print_usage; exit 1
      fi
      eval set -- "$args"

      for opt; do
        case "$opt" in
          -h|--help) print_help; exit 0 ;;
          --) shift; break ;;
          *) exit 1 ;;
        esac
      done

      if [ $# -eq 0 ]; then
        print_help; exit 0
      elif [ $# -ne 1 ]; then
        >&2 echo "Error: expected input file"; print_usage; exit 1
      fi

      "${pkgs.jq}/bin/jq" -f "${./circuitsim-history.jq}" "$1"
    '';

    # Append a path to the rpath of an executable
    add-rpath = with pkgs; writeShellScriptBin "add-rpath" ''
      set -euo pipefail

      print_usage() { >&2 echo "Usage: add-rpath [-a|--append] <path> <files...>"; }
      if ! args=$(getopt -o ah --long append,help -n csrh -- "$@"); then
        print_usage; exit 1
      fi
      eval set -- "$args"

      appendType="prepend"
      for opt; do
        case "$opt" in
          -h|--help) print_usage; exit 0 ;;
          -a|--append) appendType="append"; shift; break ;;
          --) shift; break ;;
          *) exit 1 ;;
        esac
      done

      if [ $# -lt 2 ]; then
        print_usage; exit 1
      fi

      path="$1"
      shift
      for f in "$@"; do
        cur_rpath="$(patchelf --print-rpath "$f")"
        if [ "$appendType" = "prepend" ]; then
          new_rpath="$path:$cur_rpath"
        elif [ "$appendType" = "append" ]; then
          new_rpath="$cur_rpath:$path"
        fi
        ${patchelf}/bin/patchelf --set-rpath "$new_rpath" "$f"
      done
    '';

    # Run commands via 'nix shell'
    comma = with pkgs; writeShellScriptBin "," ''
      set -euo pipefail

      usage() {
        >&2 echo "Usage: , <package> <executable> [-- <args>...]"
      }

      if [ $# -lt 1 ]; then
        usage; exit 1
      fi

      if [[ "$1" =~ ^.*#(.*)$ ]]; then
        package="$1"
        if [ $# -lt 2 ] || [ "$2" = "--" ]; then
          executable="''${BASH_REMATCH[1]}"
        else
          executable="$2"
        fi
      else
        package="nixpkgs#$1"
        if [ $# -lt 2 ] || [ "$2" = "--" ]; then
          executable="$1"
        else
          executable="$2"
        fi
      fi

      shift
      [ $# -gt 0 ] && shift
      [ $# -gt 0 ] && [ "$1" = "--" ] && shift

      nix shell "$package" -c "$executable" "$@"
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

      delay="''${1:-59}"

      while :; do
        ${pkgs.xdotool}/bin/xdotool mousemove_relative -- 1 0
        sleep "$delay"
        ${pkgs.xdotool}/bin/xdotool mousemove_relative -- -1 0
        sleep "$delay"
      done
    '';

    # Hack to get firefox to open like chrome's --app
    firefox-app = pkgs.writeShellScriptBin "firefox-app" ''
      set -euo pipefail

      profile_path="$HOME/.local/share/firefox-profiles/app"
      chrome_path="$profile_path/chrome"

      if [ ! -d "$profile_path" ]; then
        echo "Initializing Firefox app mode..."
        mkdir -p "$profile_path" "$chrome_path"

        echo "Enabling userChrome.css..."
        cat << EOF >> "$profile_path"/prefs.js
      user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
      EOF

        echo "Writing custom userChrome.css..."
        cat << EOF > "$chrome_path"/userChrome.css
      @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");
      #TabsToolbar { visibility: collapse !important; }
      #BookmarksToolbar { visibility: collapse !important; }
      #nav-bar { visibility: collapse !important; }
      #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
        visibility: collapse !important;
      }
      EOF

        echo "Done."
      fi

      ${pkgs.firefox}/bin/firefox -profile "$profile_path" "$@"
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
          ghc="ghc8104"
          args="$*"
          ;;
      esac
      ${nix}/bin/nix-shell -p "haskell.packages.$ghc.ghcWithPackages (p: with p; [ $args ])"
    '';

    # Start a hoogle server, usually in a nix-shell
    hoogleServer = with pkgs; writeShellScriptBin "hoogleserver" ''
      hoogle server --port=''${1:-8080} --local --haskell
    '';
  };
in builtins.attrValues scripts
