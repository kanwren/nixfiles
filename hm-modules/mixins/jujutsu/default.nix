{ pkgs, config, lib, ... }:

let
  cfg = config.mixins.jujutsu;

  jj-helpers-lib = pkgs.writers.writeBash "jj-helpers-lib" ''
    shopt -s -o errexit nounset pipefail

    change_ids() {
      jj log --ignore-working-copy --revisions "$1" --reversed --no-graph --template 'change_id.short() ++ "\n"'
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
      jj log --ignore-working-copy --revisions "$1" --no-graph --template 'description'
    }

    revset() {
      change_ids "$1" | jq --null-input --raw-input --raw-output '
        [inputs] | if length == 0 then "none()" else join("|") end
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

    log_and_run() {
      printf '\x1b[1;32m$ '
      printf '%s' "$(escape "''${1}")"
      for arg in "''${@:2}"; do printf ' %s' "$(escape "''${arg}")"; done
      printf '\x1b[0m\n'
      "$@"
    }
  '';
in
{
  options.mixins.jujutsu.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the jujutsu mixin";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.difftastic
    ];

    programs.jujutsu = {
      enable = true;

      settings = {
        user.name = "Nicole Wren";

        ui = {
          allow-filesets = true;
          pager = "less -RF";
          paginate = "auto";
          default-command = "worklog";
          diff-editor = ":builtin";
          diff.tool = "difftastic";
          merge-editor = "vimdiff";
        };

        merge-tools = {
          vimdiff = {
            program = "nvim";
            # similar to the default, but opens files in a different order to
            # preserve commands like `1do`.
            merge-args = [ "-d" "-M" "$left" "$base" "$right" "$output" "-c" "$wincmd w | wincmd J | set modifiable write" ];
            merge-tool-edits-conflict-markers = true;
          };

          difftastic = {
            program = "${pkgs.difftastic}/bin/difft";
            diff-args = [ "--color=always" "$left" "$right" ];
          };
        };

        revsets = {
          log = "@ | ancestors(immutable_heads().., 2) | heads(immutable_heads())";
        };

        aliases =
          let
            mkExecAlias = program: [ "util" "exec" "--" program ];
            mkBashAlias = name: text: mkExecAlias (lib.getExe (pkgs.writers.writeBashBin "jj-${name}" text));
          in
          {
            "ui" = mkExecAlias "${pkgs.jj-fzf}/bin/jj-fzf";

            "worklog" = [ "log" "-r" "(trunk()..@):: | (trunk()..@)-" ];

            # List change IDs of changes in a revset (default '@')
            "change-id" = mkBashAlias "change-id" /* bash */ ''
              source ${jj-helpers-lib}

              main() {
                if [ $# -gt 1 ]; then
                  echo "usage: jj change-id [<revset>]" >&2
                  return 1
                fi

                jj log --ignore-working-copy --revisions "''${1-@}" --reversed --no-graph --template 'change_id ++ "\n"'
              }

              main "$@"
            '';

            # List commit IDs of changes in a revset (default '@')
            "commit-id" = mkBashAlias "commit-id" /* bash */ ''
              source ${jj-helpers-lib}

              main() {
                if [ $# -gt 1 ]; then
                  echo "usage: jj commit-id [<revset>]" >&2
                  return 1
                fi

                jj log --ignore-working-copy --revisions "''${1-@}" --reversed --no-graph --template 'commit_id ++ "\n"'
              }

              main "$@"
            '';

            # List names of bookmarks pointing to changes in a revset (default '@')
            "bookmark-names" = mkBashAlias "bookmark-names" /* bash */ ''
              source ${jj-helpers-lib}

              main() {
                if [ $# -gt 1 ]; then
                  echo "usage: jj bookmark-names [<revset>]" >&2
                  return 1
                fi
                jj log --ignore-working-copy --revisions "''${1-@}" --no-graph --template 'bookmarks.map(|b| b.name() ++ "\n").join("")'
              }

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
            "apply-split" = mkBashAlias "apply-split" /* bash */ ''
              source ${jj-helpers-lib}

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
                log_and_run jj backout --revisions "''${splitter}" --destination "''${splitter}"
                new_splitter_children="$(revset "children(''${splitter})")"
                backout="$(change_id "($new_splitter_children) ~ ($old_splitter_children)")"
                log_and_run jj describe "''${backout}" --message "backout of splitter"

                # Rebase children of $orig onto the backout
                change_ids "children(''${orig}) ~ (''${splitter})" | while read -r child; do
                  log_and_run jj rebase --source "$child" --destination "all:parents($child) ~ ''${orig} | ''${backout}"
                done

                # Squash the splitter with its parent
                log_and_run jj squash --revision "''${splitter}"

                # Rewrite the messages for the first and second halves
                if [ "''${squash_automessage}" -eq 1 ]; then
                  log_and_run jj describe "''${orig}"
                fi
                log_and_run jj describe "''${backout}"
              }

              [ $# -le 1 ] || { echo "usage: jj apply-split [<revision>]"; exit 1; }

              main "$@"
            '';

            # Run a command at every revision in a revset
            # TODO: replace when `jj run` isn't a stub anymore
            "run-job" = mkBashAlias "run-job" /* bash */ ''
              source ${jj-helpers-lib}

              log_lit_command() {
                printf '\x1b[1;32m$ %s\x1b[0m\n' "''${1}"
              }

              main() {
                register_rollback_instructions

                declare -r revset="$1"
                declare -ra cmd=("''${@:2}")
                change_ids "''${revset}" | while read -r rev; do
                  log_and_run jj edit "''${rev}"

                  log_lit_command 'cd "$(jj workspace root)"'
                  cd "$(jj --ignore-working-copy workspace root)"

                  log_and_run "''${cmd[@]}"
                done
              }

              [ $# -ge 2 ] || { echo "usage: jj run-job <revset> <command> <args>..."; exit 1; }

              main "$@"
            '';

            "flow" = mkBashAlias "flow" /* bash */ ''
              source ${jj-helpers-lib}

              # @describe Manage a branch-of-branches for a megamerge workflow
              # @meta require-tools jj

              # @cmd Move to the tip of the flow
              tip() {
                log_and_run jj new 'bookmarks(exact:"flow")'
              }

              # @cmd Manage the set of changes managed by the flow
              # @alias change,c
              changes() { :; }

              # @cmd Add a revision to the changes managed by the flow
              # @arg revset! The revision to add
              changes::add() {
                register_rollback_instructions

                local flow
                flow="$(change_ids 'present(bookmarks(exact:"flow"))')"

                if [ -n "$flow" ]; then
                  log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) | ('"$argc_revset"')'
                else
                  local old_children new_children flow_commit
                  old_children="$(revset 'children('"$argc_revset"')')"
                  log_and_run jj new --no-edit 'all:'"$argc_revset" --message 'XXX:flow'
                  new_children="$(revset 'children('"$argc_revset"')')"
                  flow_commit="$(change_id '('"$new_children"') ~ ('"$old_children"')')"
                  log_and_run jj bookmark create flow --revision "$flow_commit"
                fi
              }

              # @cmd Remove a revision from the changes managed by the flow
              # @alias rm
              # @arg revset! The revision to remove
              changes::remove() {
                register_rollback_instructions

                local num_parents flow_empty

                # If there are no parents now, we're done
                num_parents="$(change_ids 'parents(present(bookmarks(exact:"flow")))' | wc -l)"
                if [ "$num_parents" -eq 0 ]; then
                  printf '%s\n' 'nothing to do'
                  return
                fi

                # If removing the argument would remove all parents, delete the bookmark
                num_parents="$(change_ids 'parents(bookmarks(exact:"flow")) ~ ('"$argc_revset"')' | wc -l)"
                if [ "$num_parents" -eq 0 ]; then
                  flow_empty="$(change_ids 'bookmarks(exact:"flow") & none() & description(exact:"")')"
                  if [ -n "$flow_empty" ]; then
                    log_and_run jj abandon 'bookmarks(exact:"flow")'
                  fi
                  log_and_run jj bookmark delete flow
                  return
                fi

                # Otherwise, just remove the given parents
                log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) ~ ('"$argc_revset"')'
              }

              # @cmd Move a change managed by the flow to a different revision
              # @alias mv
              # @arg old! The revision to remove
              # @arg new! The revision to add
              changes::move() {
                register_rollback_instructions
                log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) ~ ('"$argc_old"') | ('"$argc_new"')'
              }

              # @cmd Rebase all changes managed by the flow onto a destination
              # @arg destination! Revision of the new base for changes
              rebase() {
                register_rollback_instructions
                log_and_run jj rebase --source 'all:roots(('"$argc_destination"')..bookmarks(exact:"flow"))' --destination "$argc_destination"
              }

              # @cmd Push all flow-managed branches
              push() {
                log_and_run jj git push --revisions 'all:trunk()..parents(bookmarks(exact:"flow"))'
              }

              eval "$(${pkgs.argc}/bin/argc --argc-eval "$0" "$@")"
            '';
          };

        revset-aliases = {
          # graph utilities
          "symdiff(x, y)" = "(x ~ y) | (y ~ x)"; # commits in either x or y, but not both
          "lr(x, y)" = "fork_point(x | y)..(x | y)"; # lr(x, y) is what 'git log' calls x...y
          "vee(x, y)" = "fork_point(x | y) | (fork_point(x | y)..(x | y))";

          # work utilities
          "named()" = "bookmarks() | remote_bookmarks() | tags() | trunk()";
          "merged()" = "ancestors(named())";
          "unmerged()" = "~merged()";

          # commit info
          "user(x)" = "author(x) | committer(x)";
          "mine()" =
            let
              names = [
                "Nicole Wren"
                "Nicole Prindle"
              ];
              emails = [
                "nicole@wren.systems"
                "nprindle18@gmail.com"
                "wrenn@squareup.com"
                "nprindle@squareup.com"
              ];
              toAuthor = x: "author(exact:${builtins.toJSON x})";
            in
            builtins.concatStringsSep " | " (builtins.map toAuthor (emails ++ names));
        };

        templates = {
          draft_commit_description = ''
            concat(
              description,
              surround(
                "\nJJ: Files:\n", "",
                indent("JJ:     ", diff.summary()),
              ),
              "\n",
              "JJ: ignore-rest\n",
              diff.git(),
            )
          '';
        };

        template-aliases = { };

        git = {
          push-bookmark-prefix = lib.mkDefault "kanwren/push-";
          private-commits = lib.mkDefault ''description(glob-i:"XXX:*")'';
        };
      };
    };
  };
}
