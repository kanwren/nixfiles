{ symlinkJoin, writers }:

let
  jj-helpers-lib = writers.writeBash "jj-helpers-lib" ''
    shopt -s -o errexit nounset pipefail

    change_ids() {
      jj log --revisions "$1" --reversed --no-graph --template 'change_id ++ "\n"'
    }

    change_id() {
      declare ids
      ids="$(change_ids "$1")"
      if [ "$(wc -l <<<"$ids")" -ne 1 ]; then
        echo "invalid revset: $1 should have exactly one revision" >&2
        return 1
      fi
      printf '%s\n' "$ids"
    }

    description() {
      jj log --revisions "$1" --no-graph --template 'description'
    }

    revset() {
      change_ids "$1" | jq --null-input --raw-input --raw-output '
        [inputs] | if length == 0 then "empty()" else join("|") end
      '
    }
  '';
in

symlinkJoin {
  name = "jj-helpers";
  paths = builtins.attrValues {
    # Move the first change to a position after the second change.
    "jj.pluck" = writers.writeBashBin "jj.pluck" ''
      source ${jj-helpers-lib}

      main() {
        declare rev target
        rev="$(change_id "$1")"
        target="$(change_id "$2")"

        # move 1 on top of 2
        jj rebase --revisions "''${rev}" --destination "''${target}"

        # move children of 2 on top of 1, if there are any
        change_ids "children(''${target}) ~ (''${rev})" | while read -r child; do
          jj rebase --source "''${child}" --destination "all:parents(''${child}) ~ (''${target}) | (''${rev})"
        done
      }

      [ $# -eq 2 ] || { echo "usage: $0 <source> <destination>"; exit 1; }

      main "$@"
    '';

    # List the change IDs for a revset ('@' by default)
    "jj.id" = writers.writeBashBin "jj.id" ''
      source ${jj-helpers-lib}

      main() {
        change_ids "''${1-@}"
      }

      [ $# -le 1 ] || { echo "usage: $0 [<revision>]"; exit 1; }

      main "$@"
    '';

    # Add empty commits before and after a commit, to guarantees that a commit has
    # only one parent and only one child.
    "jj.isolate" = writers.writeBashBin "jj.isolate" ''
      source ${jj-helpers-lib}

      main() {
        declare target
        target="$(change_id "$1")"
        jj new --no-edit --insert-before "''${target}"
        jj new --no-edit --insert-after "''${target}"
      }

      [ $# -eq 1 ] || { echo "usage: $0 <revision>"; exit 1; }

      main "$@"
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
      source ${jj-helpers-lib}
      shopt -s -o xtrace

      main() {
        declare splitter orig
        splitter="$(change_id "$1")"
        orig="$(change_id "parents($splitter)")"

        declare -i squash_automessage=0
        [ -z "$(description "''${splitter}")" ] && squash_automessage=1
        [ -z "$(description "''${orig}")" ]     && squash_automessage=1

        # Invert the changes in $splitter
        declare old_splitter_children new_splitter_children backout
        old_splitter_children="$(revset "children(''${splitter})")"
        jj backout --revision "''${splitter}" --destination "''${splitter}"
        new_splitter_children="$(revset "children(''${splitter})")"
        backout="$(change_id "($new_splitter_children) ~ ($old_splitter_children)")"
        jj describe "''${backout}" --message "backout of splitter"

        # Rebase children of $orig onto the backout
        change_ids "children(''${orig}) ~ (''${splitter})" | while read -r child; do
          jj rebase --source "$child" --destination "all:parents($child) ~ ''${orig} | ''${backout}"
        done

        # Squash the splitter with its parent
        jj squash --revision "''${splitter}"

        # Rewrite the messages for the first and second halves
        if [ "''${squash_automessage}" -eq 1 ]; then
          jj describe "''${orig}"
        fi
        jj describe "''${backout}"
      }

      [ $# -eq 1 ] || { echo "usage: $0 <revision>"; exit 1; }

      main "$@"
    '';

    # Generates a new change ID for a revision.
    "jj.recreate" = writers.writeBashBin "jj.recreate" ''
      source ${jj-helpers-lib}

      main() {
        change_ids "$1" | while read -r rev; do
          jj new --quiet --no-edit --insert-before "$rev"
          jj squash --quiet --revision "$rev"
        done
      }

      [ $# -eq 1 ] || { echo "usage: $0 <revset>"; exit 1; }

      main "$@"
    '';

    # Run a command at every revision in a revset
    "jj.run" = writers.writeBashBin "jj.run" ''
      source ${jj-helpers-lib}

      main() {
        declare -r revset="$1"
        declare -ra cmd=("''${@:2}")
        change_ids "''${revset}" | while read -r rev; do
          jj edit "''${rev}"
          "''${cmd[@]}"
        done
      }

      [ $# -ge 2 ] || { echo "usage: $0 <revset> <command> <args>..."; exit 1; }

      main "$@"
    '';
  };
}
