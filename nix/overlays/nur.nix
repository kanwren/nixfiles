# Nix User Repository
self: super:

let
  nurSrc = super.fetchFromGitHub {
    owner = "nix-community";
    repo = "NUR";
    rev = "f1250d0f80da8f34dc1b0d39cbccb6c5835e878d";
    sha256 = "0smkp2xrzm22spxf9byn67sfhdsx8sfralbigkyjchrgp1623mya";
  };
in {
  nur = import nurSrc { pkgs = self; };
}
