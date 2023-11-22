use_system_rebuild := "false"

default:
    @just --list

[private]
hecate-system:
    nix build --no-link '.#nixosConfigurations.hecate.config.system.build.toplevel'

hecate COMMAND='build': hecate-system
    {{ quote(if use_system_rebuild == "true" { 'nixos-rebuild' } else { `nix build --no-link --print-out-paths '.#nixosConfigurations.hecate.config.system.build.toplevel'` / "sw" / "bin" / "nixos-rebuild" }) }} --flake '.#hecate' {{ quote(COMMAND) }}

[private]
caspar-system:
    nix build --no-link '.#darwinConfigurations.caspar.system'

caspar COMMAND='build': caspar-system
    {{ quote(if use_system_rebuild == "true" { 'darwin-rebuild' } else { `nix build --no-link --print-out-paths '.#darwinConfigurations.caspar.system'` / "sw" / "bin" / "darwin-rebuild" }) }} --flake '.#caspar' {{ quote(COMMAND) }}
