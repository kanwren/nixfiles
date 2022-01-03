{ pkgs, lib, custom, ... }:

{
  environment = {
    systemPackages = [
      custom.pkgs.neovim-with-plugin-deps
    ];

    variables = {
      # Make neovim the default editor
      EDITOR = "nvim";
      VISUAL = "nvim";
    };
  };
}
