{
  lib,
  fetchFromGitHub,
  buildGoModule,
}:
buildGoModule rec {
  pname = "envtpl";
  version = "2.0.0";

  src = fetchFromGitHub {
    owner = "kanwren";
    repo = "envtpl";
    rev = "v${version}";
    hash = "sha256-6FSsU8yJABm8mPA8MVn2TjaBQU5nJY144bgIZUrELns=";
  };

  subPackages = ["cmd/envtpl"];

  vendorHash = null;

  meta = with lib; {
    description = "Render Go templates on the command line with shell environment variables";
    homepage = "https://github.com/subfuzion/envtpl";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
