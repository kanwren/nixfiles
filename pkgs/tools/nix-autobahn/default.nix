{ fetchFromGitHub
, naersk
, fenix
}:

let
  src = fetchFromGitHub {
    owner = "wucke13";
    repo = "nix-autobahn";
    rev = "1ed950e5ad620dc82f9109fec0ad8e2351ce9696";
    sha256 = "1mvwbgfpgfcx37aq2fnn2zbw8cw4vw45l2686lf11qm1f1ycc2xi";
  };
  toolchain = with fenix; combine [
    minimal.rustc
    minimal.cargo
    targets.x86_64-unknown-linux-musl.latest.rust-std
  ];
  naersk-lib = naersk.override {
    cargo = toolchain;
    rustc = toolchain;
  };
in
naersk-lib.buildPackage src

