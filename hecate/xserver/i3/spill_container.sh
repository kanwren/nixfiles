#! @runtimeShell@
PATH="@i3@/bin:@jq@/bin${PATH:+:${PATH}}"

set -euo pipefail

define() { IFS=$'\n' read -r -d '' "${1}" || true; }

define query_parent_children <<'EOF'
.
| (recurse(.nodes[]) | select(.nodes | any(.focused))) as $parent
| ($parent.nodes[] | select(.focused)) as $focused
| { parent: $parent.id, children: ($focused.nodes | map(.id) | reverse) }
EOF

layout="$(i3-msg -t get_tree | jq "$query_parent_children")"

i3-msg -t command "[con_id=\"$(echo "$layout" | jq ".parent")\"] mark _i3_spill_dest_con"
while IFS= read -r id; do
  if [ -n "$id" ]; then
    i3-msg -t command "[con_id=\"$id\"] move container to mark _i3_spill_dest_con"
  fi
done <<< "$(echo "$layout" | jq -r ".children[]")"
i3-msg -t command "unmark _i3_spill_dest_con"
