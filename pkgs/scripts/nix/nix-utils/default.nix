{ naersk
, fenix
, lib
}:

let
  toolchain = with fenix;
    combine [
      minimal.rustc
      minimal.cargo
    ];
  naersk-lib = naersk.override {
    cargo = toolchain;
    rustc = toolchain;
  };
in
naersk-lib.buildPackage {
  src = ./.;
  meta = with lib; {
    description = "Print nix garbage collector roots that still exist";
    platforms = platforms.linux;
  };
}
