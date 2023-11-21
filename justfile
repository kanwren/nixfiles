default:
    just --list

hecate COMMAND='build':
    #!/bin/sh
    set -eu
    result=$(nix build --no-link --print-out-paths '.#nixosConfigurations.hecate.config.system.build.toplevel')
    "$result/sw/bin/nixos-rebuild" --flake '.#hecate' {{COMMAND}}

caspar COMMAND='build':
    #!/bin/sh
    set -eu
    result=$(nix build --no-link --print-out-paths '.#darwinConfigurations.caspar.system')
    "$result/sw/bin/darwin-rebuild" --flake '.#caspar' {{COMMAND}}
