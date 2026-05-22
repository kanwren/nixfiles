# Generate a fake go.mod/go.sum in a package in a monorepo, so that the
# versioned dependency tree for a subpackage can be pruned and analyzed
# directly.
#
# After running, you can use a command like this to list all transitive
# dependencies and their versions:
#
#   $ go mod graph | awk 'BEGIN{OFS="\n"} {$1=$1; print $0}' | awk -F@ '/@/{$1=$1; print $0}' | sort -Vu

{
  coreutils,
  jq,
  writeShellApplication,
}:

writeShellApplication {
  name = "fake-go-mod";

  runtimeInputs = [
    coreutils
    jq
  ];

  bashOptions = [
    "errexit"
    "nounset"
    "pipefail"
  ];

  text = /* bash */ ''
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
}
