#!/usr/bin/env bash

shopt -so errexit
shopt -so nounset
shopt -so pipefail

usage() {
  >&2 echo 'deploy-remote.sh <name> <login> [<command>]'
}

log_and_run() {
  (
    set -x
    "$@"
  )
}

main() {
  if [ "$#" -ne 2 ] && [ "$#" -ne 3 ]; then
    usage
    return 1
  fi

  name="$1"
  login="$2"
  command="${3:-switch}"

  # Build the configuration and copy the result over
  out_path="$(log_and_run nix build --print-build-logs --print-out-paths --no-link '.#nixosConfigurations.'"$name"'.config.system.build.toplevel')"
  log_and_run nix copy --to ssh-ng://"$login" "$out_path"
  log_and_run ssh -t "$login" sudo "$out_path"/bin/switch-to-configuration "$command"
}

main "$@"
