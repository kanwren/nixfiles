{ system, stdenv, lib }:

let
  version = "0.1.2";
  frumBinaries = rec {
    "x86_64-darwin" = builtins.fetchTarball {
      url = "https://github.com/TaKO8Ki/frum/releases/download/v${version}/frum-v${version}-x86_64-apple-darwin.tar.gz";
      sha256 = "0aymcyfcqfhn98qyqwdfmsh9crjly99qlk93szi03xk61j82dykf";
    };
    "aarch64-darwin" = x86_64-darwin;
    "x86_64-linux" = builtins.fetchTarball {
      url = "https://github.com/TaKO8Ki/frum/releases/download/v${version}/frum-v${version}-x86_64-unknown-linux-musl.tar.gz";
      sha256 = "0fcym6ngj91hz35yh68w63dkj7q4nnyk2w9dnrr3l1cq1z0k04qw";
    };
    "aarch64-linux" = builtins.fetchTarball {
      url = "https://github.com/TaKO8Ki/frum/releases/download/v${version}/frum-v${version}-arm-unknown-linux-gnueabihf.tar.gz";
      sha256 = "1sxg6m97bcx8nv632x4jf415p35dq4z54sa5zbzgwnl445dkkbs6";
    };
  };
in

stdenv.mkDerivation {
  pname = "frum";
  inherit version;
  src = frumBinaries.${system}
    or (builtins.throw "unsupported system: ${system}");

  installPhase = ''
    mkdir -p "$out/bin"
    install -Dm755 frum "$out/bin/frum"
  '';

  meta = with lib; {
    description = "A little bit fast and modern Ruby version manager written in Rust";
    homepage = "https://github.com/TaKO8Ki/frum";
    license = licenses.mit;
    platforms = builtins.attrNames frumBinaries;
  };
}
