{ naersk
, fenix
, fetchFromGitHub
, pkgsStatic
, hello
, lib
}:

let
  src = fetchFromGitHub {
    owner = "fornwall";
    repo = "rust-script";
    rev = "ce508bad02a11d574657d2f1debf7e73fca2bf6e";
    sha256 = "sha256-YMoE8voFJimB1KAVN9/Dx9WbhW464iDq2CFxee0L1q4=";
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
