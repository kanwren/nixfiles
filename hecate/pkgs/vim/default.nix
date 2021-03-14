{ pkgs, ... }:

{
  environment = {
    systemPackages = (with pkgs; [
      (neovim.override {
        viAlias = true;
        vimAlias = true;
      })
      xxd

      rnix-lsp
      texlab
      clang-tools

      haskell-language-server

      # For building some plugins
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
