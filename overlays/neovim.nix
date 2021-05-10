{ neovim, ... }:

self: super:

{
  neovim-unwrapped = super.neovim-unwrapped.overrideAttrs (old: {
    src = neovim;
    buildInputs = (old.buildInputs or []) ++ [super.tree-sitter];
  });
}

