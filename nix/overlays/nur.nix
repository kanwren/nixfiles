# Nix User Repository
self: super:

let
  nurSrc = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "NUR";
    rev = "97456cb63c0e51159b14cad6cc5b8b752d1a3a73";
    sha256 = "1dbl1h9wb6srhj8rnaqwgf14661pzdrbsq2zlsy2ihlgfqgfp6s4";
  };
in {
  nur = import nurSrc { pkgs = self; };
}
