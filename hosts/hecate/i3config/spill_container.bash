#! @runtimeShell@
# shellcheck shell=bash

PATH="@i3@/bin:@jq@/bin${PATH:+:${PATH}}"

set -euo pipefail

declare -r description='Spill windows from i3 containers'

declare -r usage_text='USAGE:
    spill_container (single|multi)'

print_help() { >&2 printf '%s\n\n%s\n' "${description}" "${usage_text}"; }
print_usage() { >&2 printf '%s\n' "${usage_text}"; }

i3_tree() {
	i3-msg -t get_tree
}

i3_command() {
	declare -r command="${1?i3_command: missing argument: command}"
	i3-msg -t command "${command}"
}

i3_command_con() {
	declare -r con_id="${1?i3_command_con: missing argument: con_id}"
	declare -r command="${2?i3_command_con: missing argument: command}"
	i3_command '[con_id="'"${con_id}"'"] '"${command}"
}

# Move the currently-focused subtree up one level of the tree, out of its parent
# container, to its grandparent.
single() {
	declare -r layout="$(i3_tree | jq --compact-output '
		(recurse(.nodes[]) | select(.nodes | any(.nodes | any(.focused)))) as $grandparent
		| ($grandparent.nodes[] | select(.nodes | any(.focused))) as $parent
		| {
			grandparent_id: $grandparent.id,
			parent_type: $parent.type,
			grandparent_type: $grandparent.type
		}
	')"

	declare -r grandparent_type="$(jq --raw-output '.grandparent_type' <<< "${layout}")"
	declare -r parent_type="$(jq --raw-output '.parent_type' <<< "${layout}")"
	declare -r grandparent_id="$(jq --raw-output '.grandparent_id' <<< "${layout}")"

	# We can reparent the current subtree to another container (except for
	# workspace containers, see below), or to the top level of a workspace,
	# which is basically a container but doesn't support orientation or other
	# manipulation. Nothing else can hold a window.
	[ "${grandparent_type}" = 'con' ] || [ "${grandparent_type}" = 'workspace' ] || return

	# Each output has a container that holds all of its workspaces. Don't try to
	# move the current subtree from the top level of the workspace (from a
	# parent of type "workspace") into this container, as this is an error.
	[ "${parent_type}" = 'con' ] || return

	i3_command_con "${grandparent_id}" 'mark --add _i3_spill_dest_con'
	i3_command 'move container to mark _i3_spill_dest_con'
	i3_command 'unmark _i3_spill_dest_con'
}

# Move all child subtrees of the currently-focused container into its parent.
# In terms of the tree, this effectively deletes a container node, spilling all
# of its children into the parent.
multi() {
	declare -r layout="$(i3_tree | jq --compact-output '
		(recurse(.nodes[]) | select(.nodes | any(.focused))) as $parent
		| ($parent.nodes[] | select(.focused)) as $focused
		| {
			parent_id: $parent.id,
			focused_type: $focused.type,
			child_ids: ($focused.nodes | map(.id) | reverse)
		}
	')"

	declare -r focused_type="$(jq --raw-output '.focused_type' <<< "${layout}")"
	declare -r parent_id="$(jq --raw-output '.parent_id' <<< "${layout}")"
	declare -ra child_ids="($(jq --raw-output '.child_ids | @sh' <<< "${layout}"))"

	# Only containers can be spilled, and all containers have parents that they
	# can spill into (except the container of workspaces, which can't be
	# selected anyway).
	[ "${focused_type}" = "con" ] || return

	i3_command_con "${parent_id}" 'mark --add _i3_spill_dest_con'

	for id in "${child_ids[@]}"; do
		[ -n "${id}" ] || continue
		i3_command_con "${id}" 'move container to mark _i3_spill_dest_con'
	done

	i3_command 'unmark _i3_spill_dest_con'
}

main() {
	if [ $# -eq 0 ]; then
		print_help
		return 0
	elif [ $# -gt 1 ]; then
		>&2 printf 'error: invalid arguments\n'
		print_usage
		return 1
	fi

	declare -r action="${1}"

	case "${action}" in
		single)
			single
			;;
		multi)
			multi
			;;
		*)
			>&2 printf 'error: invalid action: %s\n' "${action}"
			return 1
			;;
	esac
}

main "$@"
