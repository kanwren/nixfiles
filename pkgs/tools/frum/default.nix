{ lib
, fetchFromGitHub
, openssl_3
, readline
, libyaml
, zlib
, pkg-config
, fenix
, naersk
,
}:
let
  toolchain = fenix.minimal.toolchain;
  naersk-lib = naersk.override {
    cargo = toolchain;
    rustc = toolchain;
  };
in
naersk-lib.buildPackage {
  pname = "frum";
  src = fetchFromGitHub {
    owner = "TaKO8Ki";
    repo = "frum";
    rev = "ffc76de236aae595f389de1c89c87ce996cd52c3";
    hash = "sha256-efMiZGNNsewPChnOz4+VpqsE4sU2GWnxAaqR6yvxX2M=";
  };

  buildInputs = [ openssl_3.dev readline libyaml zlib ];
  nativeBuildInputs = [ pkg-config ];

  # install completions
  postInstall = ''
    mkdir -p "$out/share/bash-completion/completions"
    "$out/bin/frum" completions --shell=bash > "$out/share/bash-completion/completions/frum"

    mkdir -p "$out/share/zsh/site-functions"
    "$out/bin/frum" completions --shell=zsh > "$out/share/zsh/site-functions/_frum"

    mkdir -p "$out/share/fish/vendor_completions.d"
    "$out/bin/frum" completions --shell=fish > "$out/share/fish/vendor_completions.d/frum.fish"
  '';

  meta = with lib; {
    description = "A little bit fast and modern Ruby version manager written in Rust";
    homepage = "https://github.com/TaKO8Ki/frum";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
