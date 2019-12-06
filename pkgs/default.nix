{ config, pkgs, ... }:

with rec {
  scripts = import ./scripts.nix { inherit pkgs; };

  baseSystemPackages = with pkgs; [
    nix-prefetch-git

    wget
    curl
    git
    killall
    manpages
    xclip
    tldr
    ag
    tmux
    alacritty

    feh
    zathura

    firefox
  ];
};

{
  imports = [
    ./vim/default.nix
    ./bash/default.nix
  ];

  environment = {
    systemPackages = baseSystemPackages ++ scripts;
  };

}
