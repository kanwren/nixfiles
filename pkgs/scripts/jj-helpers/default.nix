{ symlinkJoin, writers }:

let
  jj-helpers-lib = writers.writeBash "jj-helpers-lib" ''
    shopt -s -o errexit nounset pipefail

    change_ids() {
      jj log --revisions "$1" --reversed --no-graph --template 'change_id.short() ++ "\n"'
    }

    change_id() {
      declare ids
      ids="$(change_ids "$1")"
      if [ "$(echo "$ids" | wc -l)" -ne 1 ]; then
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

    escape() {
      if [ "$(printf '%q' "''${1}")" = "''${1}" ]; then
        printf '%s' "''${1}"
      else
        printf "'%s'" "''${1//\'/\'\"\'\"\'}"
      fi
    }

    register_rollback_instructions() {
      local op
      op="$(jj operation log --no-graph --template 'if(self.current_operation(), self.id().short(), "")')"
      trap 'printf '"'"'\x1b[1;33mTo roll back these changes, run:\x1b[0m\n\t\x1b[1;32mjj operation restore %s\x1b[0m\n'"'"' "'"''${op}"'"' EXIT
    }

    jj.log() {
      printf '\x1b[1;32m$ jj'
      for arg in "$@"; do printf " %s" "$(escape "''${arg}")"; done
      printf '\x1b[0m\n'
      jj "$@"
    }
  '';
in

symlinkJoin {
  name = "jj-helpers";
  paths = builtins.attrValues {
    # Move the first change to a position before or after the second change.
    "jj.reorder" = writers.writeBashBin "jj.reorder" ''
      source ${jj-helpers-lib}

      usage() { echo "usage: jj.reorder <source> (before|after) <destination>"; }

      before() {
        declare revs target
        revs="$(revset "$1")"
        target="$(change_id "$2")"

        # move 1 on top of 2's parents
        jj.log rebase --revisions "''${revs}" --destination "all:parents(''${target})"

        # move 2 and descendants on top of 1
        jj.log rebase --source "''${target}" --destination "all:heads(''${revs})"
      }

      after() {
        declare revs target
        revs="$(revset "$1")"
        target="$(change_id "$2")"

        # move 1 on top of 2
        jj.log rebase --revisions "''${revs}" --destination "''${target}"

        # move children of 2 on top of 1, if there are any
        change_ids "children(''${target}) ~ (''${revs})" | while read -r child; do
          jj.log rebase --source "''${child}" --destination "all:parents(''${child}) ~ (''${target}) | heads(''${revs})"
        done
      }

      main() {
        register_rollback_instructions

        case "''${2}" in
          after) after "''${1}" "''${3}" ;;
          before) before "''${1}" "''${3}" ;;
          *) echo "invalid position: ''${2}"; usage; exit 1 ;;
        esac
      }

      [ $# -eq 3 ] || { usage; exit 1; }

      main "$@"
    '';

    # List the change IDs for a revset ('@' by default)
    "jj.id" = writers.writeBashBin "jj.id" ''
      source ${jj-helpers-lib}

      main() {
        jj log --revisions "''${1-@}" --reversed --no-graph --template 'change_id ++ "\n"'
      }

      [ $# -le 1 ] || { echo "usage: jj.id [<revision>]"; exit 1; }

      main "$@"
    '';

    # List the commit IDs for a revset ('@' by default)
    "jj.commit" = writers.writeBashBin "jj.commit" ''
      source ${jj-helpers-lib}

      main() {
        jj log --revisions "''${1-@}" --reversed --no-graph --template 'commit_id ++ "\n"'
      }

      [ $# -le 1 ] || { echo "usage: jj.commit [<revision>]"; exit 1; }

      main "$@"
    '';

    # List the branch names for a revset ('@' by default)
    "jj.branch" = writers.writeBashBin "jj.branch" ''
      source ${jj-helpers-lib}

      main() {
        jj log --revisions "''${1-@}" --no-graph --template 'branches.map(|b| b.name() ++ "\n").join("")'
      }

      [ $# -le 1 ] || { echo "usage: jj.branch [<revision>]"; exit 1; }

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
        register_rollback_instructions

        declare splitter orig
        splitter="$(change_id "''${1-@}")"
        orig="$(change_id "parents($splitter)")"

        declare -i squash_automessage=0
        [ -z "$(description "''${splitter}")" ] && squash_automessage=1
        [ -z "$(description "''${orig}")" ]     && squash_automessage=1

        # Invert the changes in $splitter
        declare old_splitter_children new_splitter_children backout
        old_splitter_children="$(revset "children(''${splitter})")"
        jj.log backout --revision "''${splitter}" --destination "''${splitter}"
        new_splitter_children="$(revset "children(''${splitter})")"
        backout="$(change_id "($new_splitter_children) ~ ($old_splitter_children)")"
        jj.log describe "''${backout}" --message "backout of splitter"

        # Rebase children of $orig onto the backout
        change_ids "children(''${orig}) ~ (''${splitter})" | while read -r child; do
          jj.log rebase --source "$child" --destination "all:parents($child) ~ ''${orig} | ''${backout}"
        done

        # Squash the splitter with its parent
        jj.log squash --revision "''${splitter}"

        # Rewrite the messages for the first and second halves
        if [ "''${squash_automessage}" -eq 1 ]; then
          jj.log describe "''${orig}"
        fi
        jj.log describe "''${backout}"
      }

      [ $# -le 1 ] || { echo "usage: jj.apply-split [<revision>]"; exit 1; }

      main "$@"
    '';

    # Run a command at every revision in a revset
    "jj.run" = writers.writeBashBin "jj.run" ''
      source ${jj-helpers-lib}

      main() {
        register_rollback_instructions

        declare -r revset="$1"
        declare -ra cmd=("''${@:2}")
        change_ids "''${revset}" | while read -r rev; do
          jj.log edit "''${rev}"
          "''${cmd[@]}"
        done
      }

      [ $# -ge 2 ] || { echo "usage: jj.run <revset> <command> <args>..."; exit 1; }

      main "$@"
    '';

    "jj.flow" = writers.writeBashBin "jj.flow" ''
      source ${jj-helpers-lib}

      main() {
        jj.log new 'branches(exact:"flow")'
      }

      [ $# -eq 0 ] || { echo "usage: jj.flow"; exit 1; }

      main "$@"
    '';

    "jj.flow.manage" = writers.writeBashBin "jj.flow.manage" ''
      source ${jj-helpers-lib}

      main() {
        register_rollback_instructions

        local flow
        flow="$(change_ids 'present(branches(exact:"flow"))')"

        if [ -n "''${flow}" ]; then
          jj.log rebase --source 'branches(exact:"flow")' --destination 'all:parents(branches(exact:"flow")) | ('"''${1}"')'
        else
          local old_children new_children flow_commit
          old_children="$(revset 'children('"''${1}"')')"
          jj.log new --no-edit "all:''${1}"
          new_children="$(revset 'children('"''${1}"')')"
          flow_commit="$(change_id "(''${new_children}) ~ (''${old_children})")"
          jj.log branch create flow --revision "''${flow_commit}"
        fi
      }

      [ $# -eq 1 ] || { echo "usage: jj.flow.manage <revset>"; exit 1; }

      main "$@"
    '';

    "jj.flow.unmanage" = writers.writeBashBin "jj.flow.unmanage" ''
      source ${jj-helpers-lib}

      main() {
        register_rollback_instructions

        local num_parents flow_empty

        # If there are no parents now, we're done
        num_parents="$(change_ids 'parents(present(branches(exact:"flow")))' | wc -l)"
        if [ "''${num_parents}" -eq 0 ]; then
          printf '%s\n' 'nothing to do'
          return
        fi

        # If removing the argument would remove all parents, delete the branch
        num_parents="$(change_ids 'parents(branches(exact:"flow")) ~ ('"''${1}"')' | wc -l)"
        if [ "''${num_parents}" -eq 0 ]; then
          flow_empty="$(change_ids 'branches(exact:"flow") & empty() & description(exact:"")')"
          if [ -n "''${flow_empty}" ]; then
            jj.log abandon 'branches(exact:"flow")'
          fi
          jj.log branch delete flow
          return
        fi

        # Otherwise, just remove the given parents
        jj.log rebase --source 'branches(exact:"flow")' --destination 'all:parents(branches(exact:"flow")) ~ ('"''${1}"')'
      }

      [ $# -eq 1 ] || { echo "usage: jj.flow.unmanage <revset>"; exit 1; }

      main "$@"
    '';

    "jj.flow.remanage" = writers.writeBashBin "jj.flow.remanage" ''
      source ${jj-helpers-lib}

      main() {
        jj.log rebase --source 'branches(exact:"flow")' --destination 'all:parents(branches(exact:"flow")) ~ ('"''${1}"') | ('"''${2}"')'
      }

      [ $# -eq 2 ] || { echo "usage: jj.flow.remanage <from> <to>"; exit 1; }

      main "$@"
    '';

    "jj.flow.rebase" = writers.writeBashBin "jj.flow.rebase" ''
      source ${jj-helpers-lib}

      main() {
        declare -r target="''${1-trunk()}"
        jj.log rebase --source 'all:roots(('"''${target}"')..branches(exact:"flow"))' --destination "''${target}"
      }

      [ $# -le 1 ] || { echo "usage: jj.flow.rebase [<target>]"; exit 1; }

      main "$@"
    '';

    "jj.flow.push" = writers.writeBashBin "jj.flow.push" ''
      source ${jj-helpers-lib}

      main() {
        jj.log git push --revisions 'all:trunk()..parents(branches(exact:"flow"))'
      }

      [ $# -eq 0 ] || { echo "usage: jj.flow.push"; exit 1; }

      main "$@"
    '';
  };
}
