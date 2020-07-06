# Override nushell to allow config to be readonly
# TODO: remove after next version bump

self: super:

{
  nushell = super.rustPlatform.buildRustPackage rec {
    pname = "nushell";
    version = "unstable-2020-07-03";

    src = super.fetchFromGitHub {
      owner = pname;
      repo = pname;
      rev = "eb02c773d04c051a769b0e6cc84001edc8937bae";
      sha256 = "0w668c9r2xvg8j08n7zyzhzm7pzjiznxlk7gdfb8qvgzr1f37gd9";
    };

    cargoSha256 = "0fvhc3bbbjwmzh02czkfjgx5g2x753k5zg420ix9r35hyrh28pnd";

    nativeBuildInputs = with self; [ pkg-config python3 ];

    buildInputs = with self; [ openssl xorg.libX11 ];

    cargoBuildFlags = [ "--features stable" ];

    cargoTestFlags = [ ];

    checkPhase = let inherit (super) lib; in ''
      runHook preCheck
      cargo test ${lib.strings.concatStringsSep " " cargoTestFlags} -- ''${checkFlags} ''${checkFlagsArray+"''${checkFlagsArray[@]}"}
      runHook postCheck
    '';

    preCheck = "export HOME=$TMPDIR";

    passthru = {
      shellPath = "/bin/nu";
    };
  };
}
