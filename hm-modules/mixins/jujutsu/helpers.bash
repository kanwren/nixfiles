#!/usr/bin/env bash

# @describe Jujutsu utilities
# @meta require-tools jj,jq

shopt -s -o errexit nounset pipefail

jj() {
    @jj@ "$@"
}

jq() {
    @jq@ "$@"
}

sed() {
    @sed@ "$@"
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

trim() {
    local str="$1"
    str="${str#"${str%%[![:space:]]*}"}"
    str="${str%"${str##*[![:space:]]}"}"
    printf '%s' "$str"
}

register_rollback_instructions() {
    local op
    op="$(jj operation log --no-graph --template 'if(self.current_operation(), self.id().short(), "")')"
    trap '[ "$(jj operation log --no-graph --template '"'"'if(self.current_operation(), self.id().short(), "")'"'"')" = "'"$op"'" ] || printf '"'"'\x1b[1;33mTo roll back these changes, run:\x1b[0m\n\t\x1b[1;32mjj operation restore %s\x1b[0m\n'"'"' "'"$op"'"' EXIT
}

log_and_run() {
    printf '\x1b[1;32m$ '
    printf '%s' "$(escape "${1}")"
    for arg in "${@:2}"; do printf ' %s' "$(escape "$arg")"; done
    printf '\x1b[0m\n'
    "$@"
}

log_lit_command() {
    printf '\x1b[1;32m$ %s\x1b[0m\n' "${1}"
}

# @cmd Print the description of a change
# @arg revset=@ The revision to describe
description() {
    local id
    id="$(change_id "$argc_revset")"
    jj log --ignore-working-copy --revisions "$id" --no-graph --template 'description'
}

# @cmd Print the first line of a description of a change
# @arg revset=@ The revision to describe
subject() {
    local id
    id="$(change_id "$argc_revset")"
    jj log --ignore-working-copy --revisions "$id" --no-graph --template 'description.first_line()'
}

# @cmd Modify the descriptions for a changeset
# @arg revset! The revision(s) whose revisions should be changed
# @arg filter! A sed program with which to modify descriptions
reword() {
    register_rollback_instructions

    local revset
    revset="$argc_revset"

    local filter escaped_filter
    filter="$argc_filter"
    escaped_filter="$(escape "$filter")"

    local rev escaped_rev
    change_ids "$revset" | while read -r rev; do
        escaped_rev="$(escape "$rev")"
        log_lit_command 'jj log --revisions '"$escaped_rev"' --no-graph --template '"'"'description'"'"' | sed '"$escaped_filter"' | jj describe '"$escaped_rev"' --stdin'
        old_desc="$(jj log --revisions "$rev" --no-graph --template 'description')"
        new_desc="$(echo "$old_desc" | sed "$filter")"
        echo "$new_desc" | jj describe "$rev" --stdin
    done
}

# @cmd List change or commit IDs of changes in a revset
# @arg revset=@ The revision(s) to analyze
# @flag -c --commit Output commit IDs instead of change IDs
id() {
    local template="change_id"
    [ "${argc_commit-}" = 1 ] && template="commit_id"
    jj log --ignore-working-copy --revisions "$argc_revset" --reversed --no-graph --template "$template"' ++ "\n"'
}

# @cmd List names of bookmarks pointing to changes in a revset
# @arg revset=@ The revision(s) to analyze
bookmark-names() {
    jj log --ignore-working-copy --revisions "$argc_revset" --no-graph --template 'bookmarks.map(|b| b.name() ++ "\n").join("")' | sort -u
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
}

# @cmd Move a change managed by the flow to a different revision
# @alias mv
# @arg old! The revision to remove
# @arg new! The revision to add
flow::changes::move() {
    register_rollback_instructions
    log_and_run jj rebase --source 'bookmarks(exact:"flow")' --destination 'all:parents(bookmarks(exact:"flow")) ~ ('"$argc_old"') | ('"$argc_new"')'
}

# @cmd Clean merged changes from flow tracking post-rebase
# @alias clean
flow::changes::clean-empty() {
    register_rollback_instructions
    log_and_run jj abandon 'trunk()..parents(bookmarks(exact:"flow")) ~ descendants(trunk()..parents(bookmarks(exact:"flow")) ~ empty())'
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
    log_and_run jj git push --revisions 'all:trunk()..parents(bookmarks(exact:"flow")) ~ conflicts()'
}

declare -r note_prefix='NB. '

# @cmd Manage notes on a revision
note() { :; }

# @cmd Add a note to a revision
# @arg revset! Revset on which to add a note
# @arg notes+ Notes to add
note::add() {
    local note
    declare -a message_flags=()

    local note="${argc_notes[0]}"
    note="$(trim "$note")"
    message_flags+=('--message' "$note_prefix$note")
    for note in "${argc_notes[@]:1}"; do
        note="$(trim "$note")"
        message_flags+=("--message" "$note")
    done

    jj new --no-edit "$argc_revset" "${message_flags[@]}"
}

# @cmd List notes on a revision
# @arg revset! Revisions to check
note::list() {
    # TODO: use a proper label
    jj log --revisions 'children('"$argc_revset"') & notes()' --no-graph --template '
        label("working_copy commit_id", "note") ++ " " ++ self.change_id().shortest(8) ++ ":\n"
        ++ self.description().remove_prefix("'"$note_prefix"'").trim().lines().map(|x| "│ " ++ x).join("\n")
        ++ "\n\n"
    '
}
