{ config, pkgs, ... }:

with rec {
  scripts = import ./scripts.nix { inherit pkgs; };

  baseSystemPackages = with pkgs; [
    # Nix stuff
    nix-prefetch-git

    # CLI tools
    wget
    curl
    git
    killall
    manpages
    xclip
    tldr
    bc
    ag

    # Terminals and tooling
    alacritty
    tmux

    # Media
    feh
    zathura

    # Browsers
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
