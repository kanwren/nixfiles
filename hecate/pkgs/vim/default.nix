{ pkgs, ... }:

{
  environment = {
    systemPackages = (with pkgs; [
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

      # for building some plugins
      yarn
    ]) ++ (with pkgs.nodePackages; [
      typescript-language-server
      vscode-html-languageserver-bin
      vscode-css-languageserver-bin
      bash-language-server
      yaml-language-server
      vscode-json-languageserver-bin
    ]) ++ (with pkgs.python3Packages; [
      python-language-server
      pyls-mypy
      # pyls-isort
    ]);
    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
