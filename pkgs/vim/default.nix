{ pkgs, ... }:

{
  environment = {
    systemPackages = (with pkgs; [
      (neovim.override {
        viAlias = true;
        vimAlias = true;
      })
      rnix-lsp
      texlab
      clang-tools

      # Provided by overlay
      haskell-language-server
    ]) ++ (with pkgs.nodePackages; [
      typescript-language-server
      vscode-html-languageserver-bin
      vscode-css-languageserver-bin
      bash-language-server
      yaml-language-server
      # TODO: vscode-json-languageserver
    ]) ++ (with pkgs.python3Packages; [
      python-language-server
      pyls-mypy
      pyls-isort
    ]);
    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
