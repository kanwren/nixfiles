{ pkgs, lib, ... }:

{
  environment = {
    systemPackages = lib.concatLists [
      (with pkgs; [
        # core
        (neovim.override {
          viAlias = true;
          vimAlias = true;
        })
        xxd

        # for plugins
        fzf
        code-minimap

        # LSP
        rnix-lsp
        texlab
        clang-tools
        haskell-language-server
        rust-analyzer
      ])
      (with pkgs.nodePackages; [
        typescript-language-server
      ])
      (with pkgs.python3Packages; [
        python-language-server
        pyls-mypy
      ])
    ];

    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
