{ pkgs, ... }:

{
  environment = {
    systemPackages = [
      (pkgs.neovim.override {
        viAlias = true;
        vimAlias = true;
      })
    ];
    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
