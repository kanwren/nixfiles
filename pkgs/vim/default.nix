{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [ vim_configurable ctags ];
    shellAliases.vi = "vim";
    variables.EDITOR = "vim";
    variables.VISUAL = "vim";
  };

  # This is an option, but filetype support is managed by plugins in vim configs
  # instead of by patching vim
  # nixpkgs.config.vim = {
  #   ftNixSupport = true;
  #   pythonSupport = true;
  # };

  programs = {
    vim.defaultEditor = true;
    bash.shellAliases = {
      vi = "vim";
      svim = "sudo -E vim"; # or sudoedit
    };
  };
}
