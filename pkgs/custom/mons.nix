{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "mons";
  version = "0.8.2";

  src = pkgs.fetchgit {
    url = "https://github.com/Ventto/mons";
    rev = "0c9e1a1dddff23a0525ed8e4ec9af8f9dd8cad4c";
    sha256 = "02c41mw3g1mgl91hhpz1n45iaqk9s7mdk1ixm8yv6sv17hy8rr4w";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs; [ gnumake ];
  buildInputs = with pkgs; [ help2man ];

  configurePhase = "sed -i '7,10s/=/?=/' Makefile";

  buildPhase = "true";

  installPhase = ''
    mkdir -p "$out"

    BINDIR="$out/bin" \
    LIBDIR="$out/lib" \
    MANDIR="$out/share/man/man1" \
    LICENSEDIR="$out/share/licenses/mons" \
    make install
  '';
}

