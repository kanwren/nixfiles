# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "47cc03fae7b955fc374717f2cef611016819a158";
    sha256 = "013fwkd9hwpm7wlyld4hsmr826cklwrhgd3m7hjrxq9qpc80zp3k";
  };
in {
  cs2110 = import cs2110src {};
}
