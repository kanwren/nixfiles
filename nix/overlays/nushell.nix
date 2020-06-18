# Override nushell to allow config to be readonly
# TODO: remove after next version bump

self: super:

{
  nushell = super.rustPlatform.buildRustPackage rec {
    pname = "nushell";
    version = "unstable-2020-06-17";

    src = super.fetchFromGitHub {
      owner = pname;
      repo = pname;
      rev = "96d58094cf93962ae46729090bed057e478117f5";
      sha256 = "0lbb161hknrrdfa2gald58b69yy630mig9ksdabdnnhkksv4262s";
    };

    cargoSha256 = "01klqdlviwsvd30yc88idmij4v0p3dq9p10qx376m4ghlb22bk9x";

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
