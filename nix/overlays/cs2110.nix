# Packages from the CS2110 toolchain
self: super:

let
  sources = import ../sources.nix;
in {
  cs2110 = import sources.cs2110-nix {};
}
