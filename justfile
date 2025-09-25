set unstable := true
set shell := ["/bin/sh", "-e", "-u", "-o", "pipefail", "-c"]
set script-interpreter := ["/bin/sh", "-e", "-u", "-o", "pipefail"]

v := ""
extra_nix_flags := ""
[private]
verbose_flags := (if v != "" { " --print-build-logs --keep-going" } else { "" })
[private]
nix := "nix"
[private]
nix_command := nix + " --experimental-features 'nix-command flakes'" + verbose_flags + (if extra_nix_flags != "" { " " + extra_nix_flags } else { "" })
[private]
just := just_executable() + " --set v " + quote(v) + " --set extra_nix_flags " + quote(extra_nix_flags) + " --set nix " + quote(nix)

# Show this list
[private]
list-recipes:
    @just --list --unsorted --list-prefix '    '
    @echo "Variables:"
    @just --evaluate | while IFS= read line; do echo "    $line"; done

# Fetch new versions of all flake inputs and regenerate the flake.lock
update-inputs input="":
    {{ nix_command }} flake update{{ if input != "" { ' ' + quote(input) } else { '' } }} --commit-lock-file

# Pin an input in the flake.lock to a specific flake reference
pin-input input target:
    {{ nix_command }} flake lock --commit-lock-file --override-input {{ quote(input) }} {{ quote(target) }}

# Check flake evaluation and run all checks
check-flake:
    {{ nix_command }} flake check

# Check formatting
check-formatting:
    {{ nix_command }} build '.#checks.{{ `nix eval --impure --expr 'builtins.currentSystem'` }}.check-format'
    just --unstable --fmt --check

[private]
reformat_nix:
    {{ nix_command }} fmt -- .

[private]
reformat_just:
    just --unstable --fmt

# Run the formatter
reformat: reformat_nix reformat_just

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
    current_shell="$({{ nix-command }} shell 'nixpkgs#getent' -c getent passwd "$USER" | cut -d: -f7)"
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
