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
    rev = "09c534fecb427eec638bf7066dff939a5be5f7f6";
    sha256 = "0361p9awpf74jjfv66a3f1zvi5n59zd8jss9diydk2lrb54rr3hk";
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
