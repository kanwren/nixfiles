self: super:

let
  all-hies-src = super.fetchFromGitHub {
    owner = "infinisil";
    repo = "all-hies";
    rev = "92148680060ed68f24738128d8489f4f9387d2ff";
    sha256 = "1yb75f8p09dp4yx5d3w3wvdiflaav9a5202xz9whigk2p9ndwbp5";
  };
in {
  all-hies = import all-hies-src {};
}
