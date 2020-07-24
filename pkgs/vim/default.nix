{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      (neovim.override {
        viAlias = true;
        vimAlias = true;
      })
      texlab
      rnix-lsp
    ];
    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
