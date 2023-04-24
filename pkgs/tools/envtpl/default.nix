{ envtpl-src, lib, buildGoModule }:

buildGoModule rec {
  pname = "envtpl";
  version = "unstable";

  src = envtpl-src;
  subPackages = [ "cmd/envtpl" ];

  vendorHash = null;

  meta = with lib; {
    description = "Render Go templates on the command line with shell environment variables";
    homepage = "https://github.com/subfuzion/envtpl";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}

