set unstable
set shell := ["/bin/sh", "-e", "-u", "-o", "pipefail", "-c"]
set script-interpreter := ["/bin/sh", "-e", "-u", "-o", "pipefail"]

[private]
nix_command := "nix --experimental-features 'nix-command flakes' --print-build-logs --keep-going"

# Show this list
[private]
list-recipes:
    @just --list --unsorted --list-prefix '    '

# Fetch new versions of flake inputs
update input="":
    {{ nix_command }} flake update{{ if input != "" { ' ' + quote(input) } else { '' } }} --commit-lock-file

# Pin a flake input to a specific reference
pin-input input target:
    {{ nix_command }} flake lock --commit-lock-file --override-input {{ quote(input) }} {{ quote(target) }}

# Run all flake checks
check:
    {{ nix_command }} flake check

# Run the formatter
fmt:
    {{ nix_command }} fmt

# Rebuild the nix-index index
reindex:
    {{ nix_command }} build --no-link --print-out-paths 'nixpkgs#nix-index'
    {{ nix_command }} run 'nixpkgs#nix-index'

# Run the nix installer
install-nix force="false":
    if {{ if force == "true" { "true" } else { "! command -v nix" } }}; then {{ "curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install" }}; fi

# Change the login shell for the current user
darwin-change-shell SHELL:
    #!/bin/sh
    set -eux
    current_shell="$({{ nix_command }} shell 'nixpkgs#getent' -c getent passwd "$USER" | cut -d: -f7)"
    new_shell="/run/current-system/sw/bin/"{{ quote(SHELL) }}
    if ! test -f "$new_shell"; then
        echo "error: $new_shell not found"
        exit 1
    fi
    if ! grep --line-regex --fixed-strings "$new_shell" /etc/shells >/dev/null 2>/dev/null; then
        echo "$new_shell" | sudo tee -a /etc/shells
    fi
    if test "$current_shell" != "$new_shell"; then
        chsh -s "$new_shell"
    fi
