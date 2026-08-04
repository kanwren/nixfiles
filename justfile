set unstable
set shell := ["/bin/sh", "-e", "-u", "-o", "pipefail", "-c"]
set script-interpreter := ["/bin/sh", "-e", "-u", "-o", "pipefail"]
set positional-arguments
set lazy

nix_command := "nix --experimental-features 'nix-command flakes' --print-build-logs --keep-going"
hostname := `hostname -s`

[doc("Show this list")]
[private]
help:
    @just --list --unsorted --list-prefix '    '

[arg("persist", long="persist", short="p", value="true", help="Build and persist for next system restart, but do not activate")]
[arg("dry_run", long="dry-run", short="d", value="true", help="Build and show what would happen upon activation")]
[arg("activate", long="activate", short="a", value="true", help="Build and activate, but do not persist for next system restart")]
[doc("Apply the system configuration")]
[group("system")]
[linux]
apply activate="" persist="" dry_run="":
    {{ assert((if dry_run != "" { activate + persist } else { "" }) == "", "error: cannot use --dry-run with --activate or --persist") }}
    sudo nixos-rebuild {{ if activate != "" { if persist != "" { "switch" } else { "test" } } else if persist != "" { "boot" } else if dry_run != "" { "dry-activate" } else { "switch" } }} --flake '.#{{ hostname }}'

[arg("dry_run", long="dry-run", short="d", value="true", help="Build and show what would happen upon activation")]
[arg("activate", long="activate", short="a", value="true", help="Build and activate, but do not persist for next system restart")]
[doc("Apply the system configuration")]
[group("system")]
[macos]
apply activate="" persist="" dry_run="":
    sudo darwin-rebuild {{ if activate != "" { "activate" } else { "switch" } }}{{ if dry_run != "" { " --dry-run" } else { "" } }} --flake '.#{{ hostname }}'

[doc("Activate this configuration and persist it for next system restart")]
[group("system")]
switch: (apply "true" "true" "")

[doc("Activate this configuration")]
[group("system")]
activate: (apply "true" "" "")

[doc("Persist this configuration for next system restart")]
[group("system")]
boot: (apply "" "true" "")

[doc("Build the system configuration")]
[group("system")]
[linux]
build:
    {{ nix_command }} build --no-link --print-out-paths --print-build-logs --keep-going '.#nixosConfigurations.{{ hostname }}.config.system.build.toplevel'

[doc("Build the system configuration")]
[group("system")]
[macos]
build:
    {{ nix_command }} build --no-link --print-out-paths --print-build-logs --keep-going '.#darwinConfigurations.{{ hostname }}.system'

[doc('Enter a repl with the system configuration')]
[group("system")]
[linux]
repl:
    nix repl '.#nixosConfigurations.{{ hostname }}'

[doc('Enter a repl with the system configuration')]
[group("system")]
[macos]
repl:
    nix repl '.#darwinConfigurations.{{ hostname }}'

[doc("View the system generation history")]
[group("system")]
history:
    nix profile history --profile /nix/var/nix/profiles/system

[doc("Roll back to another generation")]
[group("system")]
[linux]
rollback generation="":
    sudo sh -c 'nix profile rollback --profile /nix/var/nix/profiles/system{{ if generation != "" { " --to " + generation } else { "" } }} && /nix/var/nix/profiles/system/bin/switch-to-configuration switch'

[doc("Roll back to another generation")]
[group("system")]
[macos]
rollback generation="":
    sudo sh -c 'nix profile rollback --profile /nix/var/nix/profiles/system{{ if generation != "" { " --to " + generation } else { "" } }} && /nix/var/nix/profiles/system/activate'

[doc("Fetch new versions of flake inputs")]
[group("flake")]
update *inputs:
    {{ nix_command }} flake update "$@" --commit-lock-file

[doc("Pin a flake input to a specific reference")]
[group("flake")]
pin-input input target:
    {{ nix_command }} flake lock --commit-lock-file --override-input {{ quote(input) }} {{ quote(target) }}

[doc("Run all flake checks")]
[group("flake")]
check:
    {{ nix_command }} flake check

[doc("Run the formatter")]
[group("flake")]
fmt:
    {{ nix_command }} fmt

[doc("Change the login shell for the current user")]
[group("utils")]
[macos]
change-shell SHELL:
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
