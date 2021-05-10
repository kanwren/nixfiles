{ stdenv
, lib
, fetchFromGitHub
, gnumake
, musl
, bison
, flex
}:

stdenv.mkDerivation {
  name = "spim";

  src = fetchFromGitHub {
    owner = "rudyjantz";
    repo = "spim-keepstats";
    rev = "9d7acf26b4fb473df839b2fce9b737351a16f77d";
    sha256 = "sha256-BjaNcT1sEke66VTfL6+9t4PHw6FtNpRzVyuFbdbZzlc=";
  };

  nativeBuildInputs = [ gnumake ];
  buildInputs = [ musl bison flex ];

  buildPhase = ''
    cd spim
    make CC=musl-gcc LDFLAGS=-static EXCEPTION_DIR="$out"/share/spim
  '';

  installPhase = ''
    mkdir -p "$out"/bin "$out"/share/spim
    make install BIN_DIR="$out"/bin EXCEPTION_DIR="$out"/share/spim
  '';

  meta = with lib; {
    description = "An assembly language MIPS R2000/R3000 simulator";
    homepage = "https://github.com/rudyjantz/spim-keepstats";
    platforms = platforms.unix;
    license = licenses.bsd3;
  };
}

