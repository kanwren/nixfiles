# Override nushell to allow config to be readonly
# TODO: remove after next version bump

self: super:

{
  nushell = super.rustPlatform.buildRustPackage rec {
    pname = "nushell";
    version = "unstable-2020-06-20";

    src = super.fetchFromGitHub {
      owner = pname;
      repo = pname;
      rev = "853d7e712049c6a2fef98da06a9f274fcc84961c";
      sha256 = "1r1m6z1rl7bicdz5lcg1srn151dy2ccj3f6m2i9zfcssq1dj1781";
    };

    cargoSha256 = "186h5kbayhxvx6bmjmvdw4qmcyrb7jfb0gw3w6fhxdraszvbzg1z";

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
