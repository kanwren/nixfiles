# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "93024df307d7af37d26965a0047bc18700b9f912";
    sha256 = "0ngv7zbg1jc5rmciwb56lks0kwbq1g1cwa3wl2y1vgchnfbbh8ia";
  };
in {
  cs2110 = import cs2110src {};
}
