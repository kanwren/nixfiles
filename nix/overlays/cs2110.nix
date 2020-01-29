# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "dd1912f9882b9c46c2154f5582afeaee3f6ec92a";
    sha256 = "1fv7anlz2hz5amsryjmvfk4kmgyj4zbvlbric803gmnnp54l0mdw";
  };
in {
  cs2110 = import cs2110src {};
}
