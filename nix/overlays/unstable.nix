self: super:

let
  unstableSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz";
  };
  unstable = import unstableSrc { inherit (self) config; };
in

{
  inherit unstable;
}
