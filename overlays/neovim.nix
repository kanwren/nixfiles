{ inputs, ... }:

final: prev:

{
  neovim-unwrapped = prev.neovim-unwrapped.overrideAttrs (old: {
    src = inputs.neovim-git;
    buildInputs = (old.buildInputs or [ ]) ++ [ prev.tree-sitter ];
  });
}

