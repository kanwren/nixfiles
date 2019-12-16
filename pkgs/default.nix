{ config, pkgs, ... }:

with rec {
  scripts = import ./scripts.nix { inherit pkgs; };

  baseSystemPackages = with pkgs; [
    # Nix stuff
    nix-prefetch-git
    cachix

    # CLI tools
    wget
    curl
    git
    killall
    manpages
    whois
    bc
    tree
    xclip

    parted
    gparted
    ntfsprogs
    mkpasswd
    openssl
    nmap

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
