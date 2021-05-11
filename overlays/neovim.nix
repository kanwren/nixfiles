{ neovim, ... }:

final: prev:

{
  neovim-unwrapped = prev.neovim-unwrapped.overrideAttrs (old: {
    src = neovim;
    buildInputs = (old.buildInputs or [ ]) ++ [ prev.tree-sitter ];
  });
}

