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
      python3Packages.python-language-server

      # Provided by overlay
      haskell-language-server
    ]) ++ (with pkgs.nodePackages; [
      typescript-language-server
      vscode-html-languageserver-bin
      vscode-css-languageserver-bin
      bash-language-server
      yaml-language-server
      # TODO: vscode-json-languageserver
    ]);
    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
