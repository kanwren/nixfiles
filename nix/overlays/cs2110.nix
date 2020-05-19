# Packages from the CS2110 toolchain
self: super:

let
  cs2110src = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "cs2110-nix";
    rev = "bf55d706c8f554d4c9552e87d562f6520c448112";
    sha256 = "1i7jkdw09v88b0jmmmhyr0vsdw1fznr7l18xwh23kswbiin2czxx";
  };
in {
  cs2110 = import cs2110src {};
}
