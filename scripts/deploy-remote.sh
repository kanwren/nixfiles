#!/usr/bin/env bash

shopt -so errexit
shopt -so nounset
shopt -so pipefail

usage() {
  >&2 echo 'deploy-remote.sh <name> <login> [<command>]'
}

declare -g tmpdir

run_local() {
  (
    set -x
    "$@"
  )
}

run_remote() {
  local login="${1?run_remote: missing arg: login}"
  run_local ssh -t "$login" -o ControlMaster=auto -o ControlPath="$tmpdir"/ssh-%n -o ControlPersist=60 -- "${@:2}"
}

deploy() {
  local name="$1"
  local login="$2"
  local command="$3"

  # Build the configuration and copy the result over
  out_path="$(run_local nix build --print-build-logs --print-out-paths --no-link '.#nixosConfigurations.'"$name"'.config.system.build.toplevel')" || return $?

  if [ "$command" = "build" ]; then
    return 0
  fi

  run_local nix store sign --recursive --key-file /etc/nix/secret-key "$out_path" || return $?
  run_local nix copy --to ssh-ng://"$login" "$out_path" || return $?
  if [[ $command =~ ^switch|boot$ ]]; then
    run_remote "$login" sudo nix-env --profile /nix/var/nix/profiles/system --set "$out_path" || return $?
  fi
  run_remote "$login" sudo "$out_path"/bin/switch-to-configuration "$command" || return $?
}

main() {
  if [ "$#" -ne 2 ] && [ "$#" -ne 3 ]; then
    usage
    return 1
  fi

  local name="$1"
  local login="$2"
  local command="${3:-switch}"

  tmpdir="$(mktemp -t -d deploy.XXXXXX)"
  local status=0
  deploy "$name" "$login" "$command" || status=$?
  rm -rf "$tmpdir"
  return "$status"
}

main "$@"
