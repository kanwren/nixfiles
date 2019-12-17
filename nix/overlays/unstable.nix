# Unstable nixpkgs channel
self: super:

let
  unstableSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz";
  };
in {
  unstable = import unstableSrc { inherit (self) config; };
}
