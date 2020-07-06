# Nix User Repository
#
# Note that only packages can be used from here; if using lib/modules/overlays,
# we must import by using builtin fetchers to avoid infinite recursion
#
# https://github.com/nix-community/nur#using-modules-overlays-or-library-functions-in-nixos

self: super:

{
  nur = import ../../common/nur.nix { pkgs = self; };
}
