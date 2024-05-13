{ lib, symlinkJoin, writers }:

symlinkJoin {
  name = "jj-helpers";
  paths = builtins.attrValues {
    # Move the first change to a position after the second change.
    "jj.pluck" = writers.writeBashBin "jj.pluck" ''
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
    "jj.id" = writers.writeBashBin "jj.id" ''
      set -euo pipefail
      [ $# -le 1 ] || { echo "usage: $0 [<revision>]"; exit 1; }
      jj log --revisions "''${1-@}" --no-graph --template 'change_id ++ "\n"' | tac
    '';

    # Add empty commits before and after a commit, to guarantees that a commit has
    # only one parent and only one child.
    "jj.isolate" = writers.writeBashBin "jj.isolate" ''
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
    "jj.apply-split" = writers.writeBashBin "jj.apply-split" ''
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
  };
}
