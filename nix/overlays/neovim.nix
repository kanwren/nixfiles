self: super:

let
  sources = import ../sources.nix;
in {
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (_: {
    src = sources.neovim;
  });
}

