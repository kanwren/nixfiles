# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "1cb125c77884acd8103810f5640027079f9a15d4";
    sha256 = "0ymd9lbzr2qfvgkdvw95gwk64bzjqrf7li9k1wdrs9qnpvyy5bmq";
  };
in {
  cs2110 = import cs2110src {};
}
