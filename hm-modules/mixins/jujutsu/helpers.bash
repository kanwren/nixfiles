#!/usr/bin/env bash

# @describe Jujutsu utilities
# @meta require-tools jj,jq

shopt -s -o errexit nounset pipefail

jj() {
    @jujutsu@/bin/jj "$@"
}

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
    if [ "$(printf '%q' "${1}")" = "${1}" ]; then
        printf '%s' "${1}"
    else
        printf "'%s'" "${1//\'/\'\"\'\"\'}"
    fi
}

register_rollback_instructions() {
    local op
    op="$(jj operation log --no-graph --template 'if(self.current_operation(), self.id().short(), "")')"
    trap 'printf '"'"'\x1b[1;33mTo roll back these changes, run:\x1b[0m\n\t\x1b[1;32mjj operation restore %s\x1b[0m\n'"'"' "'"$op"'"' EXIT
}

log_and_run() {
    printf '\x1b[1;32m$ '
    printf '%s' "$(escape "${1}")"
    for arg in "${@:2}"; do printf ' %s' "$(escape "$arg")"; done
    printf '\x1b[0m\n'
    "$@"
}

# @cmd List change IDs of changes in a revset
# @arg revset=@ The revision(s) to analyze
change-id() {
    jj log --ignore-working-copy --revisions "$argc_revset" --reversed --no-graph --template 'change_id ++ "\n"'
}

# @cmd List commit IDs of changes in a revset
# @arg revset=@ The revision(s) to analyze
commit-id() {
    jj log --ignore-working-copy --revisions "$argc_revset" --reversed --no-graph --template 'commit_id ++ "\n"'
}

# @cmd List names of bookmarks pointing to changes in a revset
# @arg revset=@ The revision(s) to analyze
bookmark-names() {
    jj log --ignore-working-copy --revisions "$argc_revset" --no-graph --template 'bookmarks.map(|b| b.name() ++ "\n").join("")'
}

log_lit_command() {
    printf '\x1b[1;32m$ %s\x1b[0m\n' "${1}"
}

# @cmd Run a command at every revision in a revset
# @arg revset! The revisions to operate on
# @arg command! The command to exec
# @arg args* Arguments to the command
run-job() {
    # TODO: replace this when "$(jj run)" isn't a stub anymore

    register_rollback_instructions

    declare -r revset="$argc_revset"
    declare -ra cmd=("$argc_command" "${argc_args[@]}")
    change_ids "$revset" | while read -r rev; do
        log_and_run jj edit "$rev"

        log_lit_command 'cd "''$''(jj workspace root)"'
        cd "$(jj --ignore-working-copy workspace root)" || return

        log_and_run "${cmd[@]}"
    done
}

# @cmd Run pre-commit on the files changed in the specified revisions
# @arg revset=@ The revisions for which the changed files in them should be run against pre-commit
pre-commit() {
    local _files
    _files="$(jj log --revisions "${argc_revset}" --no-graph --template 'diff.files().map(|x| stringify(x.path()).escape_json())' | jq --null-input --raw-output '[inputs] | sort | unique | @sh')"
    declare -a files="(${_files})"
    command pre-commit run --files "${files[@]}"
}

# @cmd Manage a branch-of-branches for a megamerge workflow
flow() { :; }

# @cmd Move to the tip of the flow
flow::tip() {
    log_and_run jj new 'bookmarks(exact:"flow")'
}

# @cmd Manage the set of changes managed by the flow
# @alias change,c
flow::changes() { :; }

# @cmd Add a revision to the changes managed by the flow
# @arg revset! The revision to add
flow::changes::add() {
    register_rollback_instructions

    local flow
    flow="$(change_ids 'present(bookmarks(exact:"flow"))')"

    if [ "$flow" != "" ]; then
        log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) | ('"$argc_revset"')'
    else
        local old_children new_children flow_commit
        old_children="$(revset 'children('"$argc_revset"')')"
        log_and_run jj new --no-edit 'all:'"$argc_revset" --message 'xxx:flow'
        new_children="$(revset 'children('"$argc_revset"')')"
        flow_commit="$(change_id '('"$new_children"') ~ ('"$old_children"')')"
        log_and_run jj bookmark create flow --revision "$flow_commit"
    fi

    log_and_run jj simplify-parents --revisions 'bookmarks(exact:"flow")'
}

# @cmd Remove a revision from the changes managed by the flow
# @alias rm
# @arg revset! The revision to remove
flow::changes::remove() {
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
        if [ "$flow_empty" != "" ]; then
            log_and_run jj abandon 'bookmarks(exact:"flow")'
        fi
        log_and_run jj bookmark delete flow
        return
    fi

    # Otherwise, just remove the given parents
    log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) ~ ('"$argc_revset"')'
    log_and_run jj simplify-parents --revisions 'bookmarks(exact:"flow")'
}

# @cmd Move a change managed by the flow to a different revision
# @alias mv
# @arg old! The revision to remove
# @arg new! The revision to add
flow::changes::move() {
    register_rollback_instructions
    log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) ~ ('"$argc_old"') | ('"$argc_new"')'
    log_and_run jj simplify-parents --revisions 'bookmarks(exact:"flow")'
}

# @cmd Rebase all changes managed by the flow onto a destination
# @arg destination! Revision of the new base for changes
flow::rebase() {
    register_rollback_instructions
    log_and_run jj rebase --source 'all:roots(('"$argc_destination"')..bookmarks(exact:"flow"))' --destination "$argc_destination"
}

# @cmd Push all flow-managed branches
flow::push() {
    log_and_run jj git push --revisions 'all:trunk()..parents(bookmarks(exact:"flow"))'
}
