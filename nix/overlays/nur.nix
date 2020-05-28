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
    rev = "8ac25f6b5af24f829872c88fefbd6c434645a271";
    sha256 = "0wzjmx9vwsc7sb6has5vzj7nrl4cm91l01hflldr82g0v3s99nsg";
  };
in {
  nur = import nurSrc { pkgs = self; };
}
