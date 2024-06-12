set shell := ["/bin/sh", "-c"]

# Use the nixos-rebuild/darwin-rebuild command from the built target system,
# rather than the one from the current system's path

use_rebuild_from_target := "false"

# Use sudo for rebuild commands that require it

use_sudo := "true"

# Add '--experimental-features "nix-command flakes"' to the nix flags

use_experimental_features := "true"

# Extra flags to pass to the nix command, e.g. '--print-build-logs' or
# '--keep-going'

extra_nix_flags := ""

# Override this to use a nix not in the PATH

nix_executable := "nix"

# The final nix command

nix_command := nix_executable + (if use_experimental_features == "true" { " --experimental-features 'nix-command flakes'" } else { "" }) + (if extra_nix_flags != "" { " " + extra_nix_flags } else { "" })

# Show this list
_list-recipes:
    @just --list --unsorted --list-prefix '    '
    @echo "Variables:"
    @just --evaluate | while IFS= read line; do echo "    $line"; done

[private]
hecate-system: _validate-variables
    {{ nix_command }} build --no-link --print-out-paths '.#nixosConfigurations.hecate.config.system.build.toplevel'

# Run a nixos-rebuild command on hecate
hecate command="build": _validate-variables hecate-system
    {{ if use_sudo == "true" { if command =~ "^boot|switch|test$" { "sudo " } else { "" } } else { "" } }}{{ quote(if use_rebuild_from_target == "false" { 'nixos-rebuild' } else { `nix --experimental-features 'nix-command flakes' build --no-link --print-out-paths '.#nixosConfigurations.hecate.config.system.build.toplevel'` / "sw" / "bin" / "nixos-rebuild" }) }} --flake '.#hecate' {{ quote(command) }}

[private]
caspar-system: _validate-variables
    {{ nix_command }} build --no-link --print-out-paths '.#darwinConfigurations.caspar.system'

# Run a darwin-rebuild command on caspar
caspar command="build": _validate-variables caspar-system
    {{ quote(if use_rebuild_from_target == "false" { 'darwin-rebuild' } else { `nix --experimental-features 'nix-command flakes' build --no-link --print-out-paths '.#darwinConfigurations.caspar.system'` / "sw" / "bin" / "darwin-rebuild" }) }} --flake '.#caspar' {{ quote(command) }}

# Fetch new versions of all flake inputs and regenerate the flake.lock
update-inputs: _validate-variables
    {{ nix_command }} flake update --commit-lock-file

# Pin an input in the flake.lock to a specific flake reference
override-input input flake: _validate-variables
    {{ nix_command }} flake lock --override-input {{ quote(input) }} {{ quote(flake) }}

# Check flake evaluation and run all checks
check-flake: _validate-variables
    {{ nix_command }} flake check

# Check formatting
check-formatting: _validate-variables
    {{ nix_command }} build '.#checks.{{ `nix eval --impure --expr 'builtins.currentSystem'` }}.check-format'
    just --unstable --fmt --check

# Run the formatter
reformat: _validate-variables
    {{ nix_command }} fmt
    just --unstable --fmt

# List image formats supported by nixos-generators
list-image-formats: _validate-variables
    {{ nix_command }} run 'github:nix-community/nixos-generators#nixos-generate' -- --list

# Use nixos-generate to build an image for the given system
build-nixos-image format system configuration: _validate-variables
    {{ nix_command }} run 'github:nix-community/nixos-generators#nixos-generate' -- --format {{ quote(format) }} --system {{ quote(system) }} --configuration ./installers/configuration.nix

# Build a custom installer with nixos-generators
build-nixos-installer system configuration="./installers/configuration.nix":
    just build-nixos-image {{ if system =~ "^aarch64-" { "sd-aarch64-installer" } else { "install-iso" } }} {{ quote(system) }} {{ quote(configuration) }}

# Rebuild the nix-index index
reindex: _validate-variables
    {{ nix_command }} build --no-link --print-out-paths 'nixpkgs#nix-index'
    {{ nix_command }} run 'nixpkgs#nix-index'

# Run the nix installer
install-nix force="false":
    if {{ if force == "true" { "true" } else { "! command -v nix" } }}; then {{ "curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install" }}; fi

# Bootstrap caspar
bootstrap-caspar: install-nix (caspar "switch") (change-shell "fish")

# Helpers

# Validate that variables have valid values
@_validate-variables:
    {{ if use_rebuild_from_target =~ "^true|false$" { "" } else { error("invalid value for use_rebuild_from_target; expected 'true' or 'false' but got " + quote(use_rebuild_from_target)) } }}
    {{ if use_sudo =~ "^true|false$" { "" } else { error("invalid value for use_sudo; expected 'true' or 'false' but got " + quote(use_sudo)) } }}
    {{ if use_experimental_features =~ "^true|false$" { "" } else { error("invalid value for use_experimental_features; expected 'true' or 'false' but got " + quote(use_experimental_features)) } }}

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
