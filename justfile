set shell := ["/bin/sh", "-e", "-u", "-o", "pipefail", "-c"]

# Verbose builds: print everything and don't exit early on failure

v := ""

# Extra flags to pass to the nix command

extra_nix_flags := ""

# Override this to use a nix not in the PATH

nix_executable := "nix"

# The final nix command

nix_command := nix_executable + " --experimental-features 'nix-command flakes'" + (if v != "" { " --print-build-logs --keep-going" } else { "" }) + (if extra_nix_flags != "" { " " + extra_nix_flags } else { "" })

# Show this list
_list-recipes:
    @just --list --unsorted --list-prefix '    '
    @echo "Variables:"
    @just --evaluate | while IFS= read line; do echo "    $line"; done

[private]
nixos-apply target command:
    nixos-rebuild {{ if v != "" { "--print-build-logs --keep-going " } else { "" } }}--flake '.#{{ target }}' {{ quote(command) }} --ask-sudo-password

[private]
darwin-apply target command:
    {{ nix_command }} build \
        --no-link \
        --print-out-paths \
        '.#darwinConfigurations.{{ target }}.system'
    {{ if command =~ "^(switch|activate)$" { "sudo " } else { "" } }} {{ quote(`nix --experimental-features 'nix-command flakes' build --no-link --print-out-paths '.#darwinConfigurations.{{ target }}.system'` / "sw" / "bin" / "darwin-rebuild") }} --flake '.#{{ target }}' {{ quote(command) }}

# Run a nixos-rebuild command on hecate
hecate command="build": (nixos-apply 'hecate' command)

# Run a nixos-rebuild command on birdbox
birdbox command="build": (nixos-apply 'birdbox' command)

# Run a darwin-rebuild command on caspar
caspar command="build": (darwin-apply 'caspar' command)

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

# Bootstrap
bootstrap-darwin host="": install-nix (darwin-apply (if host == "" { `hostname` } else { host }) "switch") (change-shell "fish")

# Helpers

# Change the login shell for the current user
[private]
change-shell SHELL:
    #!/bin/sh
    set -eux
    current_shell="$(getent passwd "$USER" | cut -d: -f7)"
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
