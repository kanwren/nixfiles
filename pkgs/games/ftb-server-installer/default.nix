{
  buildGoModule,
  fetchFromGitHub,
}:
let
  srcHash = "sha256-FT/7Gzu34XYUF9kMb/dafJ5nmcFihGu10MEbAQ/UNpw=";
  vendorHash = "sha256-rDHUBmM4HtIKAbrrT3PpMZdcNMrVfPq293BEPyMlgPQ=";
in
buildGoModule rec {
  pname = "ftb-server-downloader";
  version = "1.0.29";
  src = fetchFromGitHub {
    owner = "FTBTeam";
    repo = "FTB-Server-Installer";
    rev = "v${version}";
    hash = srcHash;
  };

  runVend = true;
  inherit vendorHash;

  env.CGO_ENABLED = 0;

  ldflags = [
    "-s"
    "-w"
    "-X"
    "ftb-server-downloader/util.ReleaseVersion=v${version}"
  ];

  postInstall = ''
    mv $out/bin/ftb-server-downloader $out/bin/ftb-server
  '';

  meta = {
    homepage = "https://github.com/FTBTeam/FTB-Server-Installer";
    mainProgram = "ftb-server";
  };
}
