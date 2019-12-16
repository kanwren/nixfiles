# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "6861f1f20035121e01015149c0b43bf5a1097330";
    sha256 = "15if8l50ckxyv913jv71ai0d2d88svcqi0cj50nhvjz7cq22qq6q";
  };
in {
  cs2110 = import cs2110src {};
}
