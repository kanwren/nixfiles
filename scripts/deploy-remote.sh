#!/usr/bin/env bash

shopt -so errexit
shopt -so nounset
shopt -so pipefail

usage() {
  >&2 echo 'deploy-remote.sh <name> <login>'
}

main() {
  if [ "$#" -ne 2 ]; then
    usage
    return 1
  fi

  name="$1"
  login="$2"

  # Copy the configuration over
  flake="$(nix flake prefetch . --json | jq --join-output '.storePath')"
  nix copy --to ssh-ng://"$login" "$flake"

  # Build the configuration remotely
  nix build --print-build-logs --print-out-paths --no-link --store ssh-ng://"$login" "$flake"'#nixosConfigurations.'"$name"'.config.system.build.toplevel'

  # Copy our current nixos-rebuild
  nixos_rebuild="$(readlink -f "$(which nixos-rebuild)")"
  nix copy --to ssh-ng://"$login" "$nixos_rebuild"
  nix build --print-build-logs --print-out-paths --no-link --store ssh-ng://"$login" "$nixos_rebuild"

  # nixos-rebuild switch
  ssh -t "$login" "$nixos_rebuild"' --ask-sudo-password switch --flake '"$flake"'#'"$name"
}

main "$@"
