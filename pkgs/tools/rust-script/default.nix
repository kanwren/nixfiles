{ naersk
, fenix
, fetchFromGitHub
, pkgsStatic
, lib
}:

let
  src = fetchFromGitHub {
    owner = "fornwall";
    repo = "rust-script";
    rev = "0.20.0";
    sha256 = "15d4k83hckicm987iys9qkahmb27d1hnl804wr0j0mvvks9d3jsr";
  };
  toolchain = with fenix;
    combine [
      minimal.rustc
      minimal.cargo
      targets.x86_64-unknown-linux-musl.latest.rust-std
    ];
  naersk-lib = naersk.override {
    cargo = toolchain;
    rustc = toolchain;
  };
in
naersk-lib.buildPackage {
  inherit src;
  nativeBuildInputs = [ pkgsStatic.stdenv.cc ];
  CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
  CARGO_BUILD_RUSTFLAGS = "-C target-feature=+crt-static";

  meta = with lib; {
    description = "Run Rust files and expressions as scripts without any setup or compilation step";
    homepage = "https://github.com/fornwall/rust-script";
    platforms = platforms.linux;
    licenses = with licenses; [ mit asl20 ];
  };
}
