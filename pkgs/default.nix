{ config, pkgs, ... }:

with rec {
  scripts = import ./scripts.nix { inherit pkgs; };

  baseSystemPackages = with pkgs; [
    nix-prefetch-git

    bash
    wget
    curl
    git
    killall
    manpages
    xclip
    tldr
    gist
    ag
    tmux
    alacritty

    firefox
    feh
    zathura
    discord
    slack
    spotify
  ];
};

{
  imports = [
    ./vim.nix
  ];

  environment = {
    systemPackages = baseSystemPackages ++ scripts;

    interactiveShellInit = ''
      set -o vi
    '';
  };
}
