{ pkgs, lib }:

let
  addDepsToPath = deps: ''
    export PATH=${lib.makeBinPath deps}''${PATH:+:$PATH}
  '';
in

{
  # Generate a fake go.mod/go.sum in a package in a monorepo, so that the
  # versioned dependency tree for a subpackage can be pruned and analyzed directly.
  #
  # After running, you can use a command like this to list all transitive dependencies and their versions:
  # $ go mod graph | awk 'BEGIN{OFS="\n"} {$1=$1; print $0}' | awk -F@ '/@/{$1=$1; print $0}' | sort -Vu
  fake-mod = pkgs.writers.writeBashBin "fake-mod" ''
    set -euo pipefail
    ${addDepsToPath [ pkgs.coreutils pkgs.jq ]}

    die() {
      echo "$@" >&2
      exit 1
    }

    main() {
      test -f ./go.mod -o -f ./go.sum && die "go.mod and/or go.sum already exists; refusing to overwrite"

      declare -r mod_root=$(dirname "$(go env -json | jq -r '.GOMOD')")
      declare -r orig_mod=$(go mod edit -json | jq -c .)
      declare -r old_module=$(jq -r '.Module.Path' <<< "$orig_mod")

      declare -a edits=()

      # Rename the module and require the old root
      edits+=(
        -module="$old_module/$(realpath --relative-to="$mod_root" .)"
        -require="$old_module@v0.0.0-00010101000000-000000000000"
        -replace="$old_module=$(realpath --relative-to=. "$mod_root")/"
      )

      # Fix any relative paths in replace directives
      while IFS= read -r replace; do
        declare -r old=$(jq -r '.Old.Path' <<< "$replace")
        declare -r new=$(jq -r '.New.Path' <<< "$replace")
        edits+=(-replace="$old=$(realpath --relative-to=. "$mod_root/$new")/")
      done < <(jq -c '.Replace[] | select(.New | has("Version") | not)' <<< "$orig_mod")

      # Copy the root mod files, apply the edits, and prune the resulting modfile
      (
        set -x
        cp "$mod_root"/go.{mod,sum} .
        go mod edit "''${edits[@]}"
        go mod tidy
      )
    }

    main
  '';

  list-make-targets = pkgs.writers.writeBashBin "list-make-targets" ''
    set -euo pipefail
    ${addDepsToPath [ pkgs.coreutils pkgs.gnumake pkgs.gawk pkgs.gnugrep ]}
    make -pRrq | awk -v RS= -F: '/(^|\n)# Files(\n|$)/,/(^|\n)# Finished Make data base/ {if ($1 !~ "^[#.]") {print $1}}' | sort | grep '^[[:alnum:]]'
  '';

  nix-meta = pkgs.writers.writeBashBin "nix-meta" ''
    set -euo pipefail
    ${addDepsToPath [ pkgs.jq ]}
    nix eval "$1".meta --json | jq -r '
      [
        "Name:\n    \(.name // "???")\(if .mainProgram then " (`\(.mainProgram)`)" else "" end)",
        "Homepage:\n    \(.homepage // "Not found")",
        "License:\n    \(.licenses // .license | if type == "array" then map(.fullName // "???") | join(", ") elif type == "object" then .fullName // "???" else "unknown" end)",
        "Maintainers:\n    \(.maintainers | if type == "array" then map(.name // .email // .github // "???") | join(", ") elif type == "object" then .name // .email // .github // "???" else "none" end)",
        "Status:\n    \([if .broken then "broken" else empty end, if .available then "available" else "unavailable" end, if .unsupported then "unsupported" else "supported" end, if .unfree then "unfree" else "free" end, if .insecure then "insecure" else empty end] | join(", "))",
        "Description:\([.description, .longDescription] | map(select(.) | gsub("^\\s+|\\s+$"; "") | gsub("^|\n"; "\n    ")) | join("\n"))"
      ] | join("\n")
    '
  '';

  jj-relocate = pkgs.writers.writeBashBin "jj-relocate" ''
    set -euo pipefail

    change_ids() {
      jj log --revisions "$1" --no-graph --template 'change_id ++ "\n"'
    }

    [ $# -eq 2 ] || { echo "usage: $0 <source> <destination>"; exit 1; }
    [ -z "$(change_ids "$1")" ] && { echo "revision not found: $1"; exit 1; }
    [ -z "$(change_ids "$2")" ] && { echo "revision not found: $2"; exit 1; }

    # move 1 on top of 2
    jj rebase --revision "$1" --destination "$2"

    # move children of 2 on top of 1, if there are any
    if [ -n "$(change_ids "children($2) ~ ($1)")" ]; then
      jj rebase --source "all:children($2) ~ ($1)" --destination "$1"
    fi
  '';
}
