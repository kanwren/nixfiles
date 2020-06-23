# Nix User Repository
#
# Note that only packages can be used from here; if using lib/modules/overlays,
# we must import by using builtin fetchers to avoid infinite recursion
#
# https://github.com/nix-community/nur#using-modules-overlays-or-library-functions-in-nixos

self: super:

let
  nurSrc = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "NUR";
    rev = "1b9cbb063df148aa7677a1cfb31a4e49916b7c08";
    sha256 = "03xb5ayqfnan9s38j4bykmnwp2708rqrqg7djl11x8z4rp2ssjqy";
  };
in {
  nur = import nurSrc { pkgs = self; };
}
