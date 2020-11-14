self: super:

let
  sources = import ../sources.nix;
  tree-sitter-0-17-3 = (import (super.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b94e7b7d8948652a13f3159eed34499f4ca64cb9";
    sha256 = "1dhqmb5apk4rn9hiffgzmkpbfx4950ij6571qbmd1myzjvxx96fd";
  }) {}).tree-sitter;
in {
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (old: {
    src = sources.neovim;
    buildInputs = (old.buildInputs or []) ++ [tree-sitter-0-17-3];
  });
}

