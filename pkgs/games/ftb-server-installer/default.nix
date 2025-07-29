{
  buildGoModule,
  fetchFromGitHub,
}: let
  srcHash = "sha256-hdwJcX9TtNZuP/0Stc+ETJywMBj62yo5bHppeZVfQlE=";
  vendorHash = "sha256-lQfm1Oa38KdPoKcsuYpI04YlGA2xuAM7odX3dWDhmk0=";
in
  buildGoModule rec {
    pname = "ftb-server-downloader";
    version = "1.0.26";
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
