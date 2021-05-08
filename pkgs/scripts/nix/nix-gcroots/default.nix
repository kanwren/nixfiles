{ writeShellScriptBin
, addmeta
}:

let
  script = writeShellScriptBin "nix-gcroots" ''
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
in addmeta script {
  description = "Print nix garbage collector roots that still exist";
}

