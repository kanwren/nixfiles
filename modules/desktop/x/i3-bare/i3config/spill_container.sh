#! @runtimeShell@
PATH="@i3@/bin:@jq@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }
print_help() { >&2 echo -e "$description\n\n$usage_text"; }
print_usage() { >&2 echo "$usage_text"; }

description="Spill windows from i3 containers"

usage_text=""
define usage_text <<'EOF'
USAGE:
    spill_container <ACTION>

ARGS:
    <ACTION>
            Spill action to perform
EOF

if [ $# -ne 1 ]; then
  >&2 echo "Error: invalid arguments"
  print_usage
fi

action="$1"
if [ "$action" = "single" ]; then

  define query_grandparent <<'EOF'
.
| (recurse(.nodes[]) | select(.nodes | any(.nodes | any(.focused)))) as $grandparent
| ($grandparent.nodes[] | select(.nodes | any(.focused))) as $parent
| { grandparent: $grandparent.id, parent_type: $parent.type, grandparent_type: $grandparent.type }
EOF

  layout="$(i3-msg -t get_tree | jq "$query_grandparent")"

  grandparent_type="$(echo "$layout" | jq -r ".grandparent_type")"
  parent_type="$(echo "$layout" | jq -r ".parent_type")"
  # prevent moving to the container holding the workspaces
  if ([ "$grandparent_type" = "con" ] || [ "$grandparent_type" = "workspace" ]) && [ "$parent_type" = "con" ]; then
    i3-msg -t command "[con_id=\"$(echo "$layout" | jq -r ".grandparent")\"] mark --add _i3_spill_dest_con"
    i3-msg -t command "move container to mark _i3_spill_dest_con"
    i3-msg -t command "unmark _i3_spill_dest_con"
  fi

elif [ "$action" = "multi" ]; then

  define query_parent_children <<'EOF'
.
| (recurse(.nodes[]) | select(.nodes | any(.focused))) as $parent
| ($parent.nodes[] | select(.focused)) as $focused
| { parent: $parent.id, focused_type: $focused.type, children: ($focused.nodes | map(.id) | reverse) }
EOF

  layout="$(i3-msg -t get_tree | jq "$query_parent_children")"

  # Trying to move a container to the container holding the workspaces would crash i3
  focused_type="$(echo "$layout" | jq -r ".focused_type")"
  if [ "$focused_type" = "con" ]; then
    i3-msg -t command "[con_id=\"$(echo "$layout" | jq -r ".parent")\"] mark --add _i3_spill_dest_con"
    while IFS= read -r id; do
      if [ -n "$id" ]; then
        i3-msg -t command "[con_id=\"$id\"] move container to mark _i3_spill_dest_con"
      fi
    done <<< "$(echo "$layout" | jq -r ".children[]")"
    i3-msg -t command "unmark _i3_spill_dest_con"
  fi

else

  >&2 echo "Error: invalid action"
  exit 1

fi

