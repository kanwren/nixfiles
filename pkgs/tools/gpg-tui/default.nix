{ naersk
, fenix
, fetchFromGitHub
, lib
, makeWrapper
, python3
, gnupg
, libgpgerror
, gpgme
, xlibs
, useColor ? false
, useSplashScreen ? false
, installCompletions ? false
}:

let
  version = "0.1.4";
  anySettings = useColor || useSplashScreen;
  src = fetchFromGitHub {
    owner = "orhun";
    repo = "gpg-tui";
    rev = "v${version}";
    sha256 = "sha256-K09xlnyvZdnSMQ0TxGBuwE98wNw2K/ApHinxkVo/6J4=";
  };
  toolchain = with fenix;
    combine [
      minimal.rustc
      minimal.cargo
    ];
  naersk-lib = naersk.override {
    cargo = toolchain;
    rustc = toolchain;
  };
  add-flags = lib.optionalString anySettings ''
    wrapProgram "$out"/bin/gpg-tui --add-flags "${
      lib.concatStringsSep " " [
        (lib.optionalString useColor "--style colored")
        (lib.optionalString useSplashScreen "--splash")
      ]
    }"
  '';
  move-completions =
    if installCompletions then ''
      mkdir -p "$out"/share/gpg-tui
      mv "$out"/bin/completions "$out"/share/gpg-tui
    '' else ''
      rm "$out"/bin/completions
    '';
in
naersk-lib.buildPackage {
  inherit src version;
  singleStep = true;
  nativeBuildInputs = lib.optionals anySettings [ makeWrapper ];
  buildInputs = [ python3 gnupg libgpgerror gpgme xlibs.libxcb ];
  postFixup = move-completions + add-flags;
  meta = {
    description = "A Terminal User Interface for GPG";
    licenses = with lib.licenses; mit;
  };
}

