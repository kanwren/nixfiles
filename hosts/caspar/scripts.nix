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

  "nix.meta" = pkgs.writers.writeBashBin "nix.meta" ''
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

  # Move the first change to a position after the second change.
  "jj.pluck" = pkgs.writers.writeBashBin "jj.pluck" ''
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

  # List the change IDs for a revset ('@' by default)
  "jj.id" = pkgs.writers.writeBashBin "jj.id" ''
    set -euo pipefail
    [ $# -le 1 ] || { echo "usage: $0 [<revision>]"; exit 1; }
    jj log --revisions "''${1-@}" --no-graph --template 'change_id ++ "\n"'
  '';

  # Add empty commits before and after a commit, to guarantees that a commit has
  # only one parent and only one child.
  "jj.isolate" = pkgs.writers.writeBashBin "jj.isolate" ''
    set -euo pipefail
    [ $# -eq 1 ] || { echo "usage: $0 <revision>"; exit 1; }
    jj new --no-edit --insert-before "$1"
    jj new --no-edit --insert-after "$1"
  '';

  # Given a revision 'splitter', like so:
  #
  #   base - change - rest
  #             \
  #              splitter
  #
  # Treat that revision as a change to back out its parent to
  # the first half of a change, and use this to split the parent into a first
  # and second half, like this:
  #
  #   base - change1 - change2 - rest
  #
  # Where:
  # - change1 - base = splitter - base
  # - change2 - base = change - base
  #
  # This is done by backing out 'splitter', rebasing 'rest' onto that backout
  # (since 'change + Δsplitter + -Δsplitter = change'), and squashing 'change'
  # with 'splitter'.
  "jj.apply-split" = pkgs.writers.writeBashBin "jj.apply-split" ''
    set -euxo pipefail

    change_ids() {
      jj log --revisions "$1" --no-graph --template 'change_id ++ "\n"'
    }

    description() {
      jj log --revisions "$1" --no-graph --template 'description'
    }

    revset() {
      printf '%s%s' 'empty()' "$(jj log --revisions "$1" --no-graph --template '" | " ++ change_id')"
    }

    [ $# -eq 1 ] || { echo "usage: $0 <revision>"; exit 1; }

    declare -r splitter="$1"
    declare -r orig="$(change_ids "parents($splitter)")"

    [ "$(change_ids "$splitter" | wc -l)" -ne 1 ] && { echo "invalid splitter: $splitter should have exactly one revision"; exit 1; }
    [ "$(wc -l <<<"$orig")" -ne 1 ] && { echo "invalid splitter: $splitter should have exactly one parent"; exit 1; }

    declare squash_automessage=0
    [ -z "$(description "$splitter")" ] && squash_automessage=1
    [ -z "$(description "$orig")" ]     && squash_automessage=1

    # Invert the changes in $splitter
    declare -r old_splitter_children="$(revset "children($splitter)")"
    jj backout --revision "$splitter" --destination "$splitter"
    declare -r new_splitter_children="$(revset "children($splitter)")"
    declare -r backout="$(change_ids "($new_splitter_children) ~ ($old_splitter_children)")"
    jj describe "$backout" --message "backout of splitter"

    # Rebase children of $orig onto the backout
    if [ -n "$(change_ids "children($orig) ~ ($splitter)")" ]; then
      jj rebase --source "all:children($orig) ~ ($splitter)" --destination "$backout"
    fi

    # Squash the splitter with its parent
    jj squash --revision "$splitter"

    # Rewrite the messages for the first and second halves
    if [ "$squash_automessage" -eq 1 ]; then
      jj describe "$orig"
    fi
    jj describe "$backout"
  '';
}
