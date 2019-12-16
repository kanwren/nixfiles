# Nix User Repository
self: super:

{
  nur =
    let nurSrc = super.fetchFromGitHub {
      owner = "nix-community";
      repo = "NUR";
      rev = "e3d491d7ec3af4afef742650a1232f8318b01773";
      sha256 = "1rbz2jprkfgshfa8kyfd2k79rhwfgc9a6nm3w41m2gs854s9v7mw";
    };
    in import nurSrc { pkgs = self; };
}
