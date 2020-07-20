self: super:

{
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (old: {
    version = "unstable-2020-07-22";
    src = super.fetchFromGitHub {
      owner = "neovim";
      repo = "neovim";
      rev = "326b87feb6179d1b36375844eab493e0bb4dbf7f";
      sha256 = "0z5kmc76awysscd0ls6bnlaly70sbgdvnv2nvhfn7vr7nvdm710l";
    };
  });
}

