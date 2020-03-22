#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

cd "${0%/*}" || exit 1

if [ ! -f networking/interfaces.txt ]; then
  echo "Adding interfaces to networking/interfaces.txt..."
  for f in /sys/class/net/*; do
    echo "$f" >> networking/interfaces.txt
    echo "Added $f"
  done
else
  echo "networking/interfaces.txt already exists, skipping..."
fi

if [ ! -f secrets.nix ]; then
  echo "Initializing secrets.nix"
  echo "{}" > secrets.nix
else
  echo "secrets.nix already exists, skipping..."
fi

# vim: set ft=bash:
