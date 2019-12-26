# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "4f800ffbdeb4d0dc4b327a0e6b80bd30e396ef84";
    sha256 = "0lbl04s7p6gfinspxcmmsfr5yxyl8p85lxpgv8lji29abgi56aka";
  };
in {
  cs2110 = import cs2110src {};
}
