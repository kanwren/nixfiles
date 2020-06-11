# Override nushell to allow config to be readonly
# TODO: remove after next version bump

self: super:

{
  nushell = super.rustPlatform.buildRustPackage rec {
    pname = "nushell";
    version = "unstable-2020-06-11";

    src = super.fetchFromGitHub {
      owner = pname;
      repo = pname;
      rev = "a268e825aa9632b85732b19a6189b0672f3227fd";
      sha256 = "0x5a7mjpa88war66qzd8mw3yf6a9yahj5c8bdzqib5s612md2998";
    };

    cargoSha256 = "09hsxkkgnp3q6ibvdvzjiplsalfq0g6166cadhsa4s30my3i0jgj";

    nativeBuildInputs = with self; [ pkg-config python3 ];

    buildInputs = with self; [ openssl xorg.libX11 ];

    cargoBuildFlags = [ "--features stable" ];

    cargoTestFlags = [ ];

    checkPhase = let inherit (super) lib; in ''
      runHook preCheck
      echo "Running cargo cargo test ${lib.strings.concatStringsSep " " cargoTestFlags} -- ''${checkFlags} ''${checkFlagsArray+''${checkFlagsArray[@]}}"
      cargo test ${lib.strings.concatStringsSep " " cargoTestFlags} -- ''${checkFlags} ''${checkFlagsArray+"''${checkFlagsArray[@]}"}
      runHook postCheck
    '';

    preCheck = "export HOME=$TMPDIR";

    passthru = {
      shellPath = "/bin/nu";
    };
  };
}
