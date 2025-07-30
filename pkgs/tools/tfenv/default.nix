{
  stdenvNoCC,
  lib,
  fetchFromGitHub,
  makeWrapper,
  gnugrep,
  unzip,
}:
stdenvNoCC.mkDerivation rec {
  pname = "tfenv";
  version = "3.0.0";

  src = fetchFromGitHub {
    owner = "tfutils";
    repo = "tfenv";
    rev = "v${version}";
    sha256 = "sha256-2Fpaj/UQDE7PNFX9GNr4tygvKmm/X0yWVVerJ+Y6eks=";
  };

  nativeBuildInputs = [makeWrapper];

  dontConfigure = true;
  dontBuild = true;
  installPhase = ''
    install -D --mode=0644 CHANGELOG.md --target-directory="$out"
    install -D --mode=0755 bin/*        --target-directory="$out"/bin
    install -D --mode=0644 share/*      --target-directory="$out"/share
    install -D --mode=0644 lib/*        --target-directory="$out"/lib
    install -D --mode=0644 libexec/*    --target-directory="$out"/libexec
  '';

  fixupPhase = ''
    find "$out"/libexec -type f -exec chmod +x {} +
    patchShebangs --host $out/libexec

    wrapProgram "$out/bin/tfenv" \
      --prefix PATH : "${lib.makeBinPath [
      gnugrep
      unzip
    ]}" \
      --run 'export TFENV_CONFIG_DIR="''${TFENV_CONFIG_DIR:-$HOME/.cache/tfenv}"' \
      --run 'mkdir -p "''${TFENV_CONFIG_DIR}"'

    wrapProgram "$out/bin/terraform" \
      --run 'export TFENV_CONFIG_DIR="''${TFENV_CONFIG_DIR:-$HOME/.cache/tfenv}"' \
      --run 'mkdir -p "''${TFENV_CONFIG_DIR}"'
  '';

  meta = {
    description = "Terraform version manager";
    homepage = "https://github.com/tfutils/tfenv";
    platforms = lib.platforms.unix;
    licenses = [lib.licenses.mit];
  };
}
