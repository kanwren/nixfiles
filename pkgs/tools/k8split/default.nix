{ k8split-src, lib, buildGoModule }:

buildGoModule rec {
  pname = "k8split";
  version = "unstable";

  src = k8split-src;
  subPackages = [ "." ];

  vendorHash = "sha256-L0CdTy6dIpamnnNech078NWApafoVjjjyM83Cv5lbUo=";

  meta = with lib; {
    description = "A CLI for splitting multidocument yaml files into discrete documents";
    homepage = "https://github.com/brendanjryan/k8split";
    license = null;
    platforms = platforms.unix;
  };
}

