{ self }:

let
  modules = {
    bash = import ./bash;
    btop = import ./btop;
    cava = import ./cava;
    direnv = import ./direnv;
    discord = import ./discord;
    dunst = import ./dunst;
    fish = import ./fish;
    firefox = import ./firefox;
    flameshot = import ./flameshot;
    git = import ./git;
    gpg-agent = import ./gpg-agent;
    gtk = import ./gtk;
    haskell = import ./haskell;
    jujutsu = import ./jujutsu;
    kitty = import ./kitty;
    nix = import ./nix;
    rofi = import ./rofi;
    rust = import ./rust;
    spotify = import ./spotify;
    tmux = import ./tmux;
    vscode = import ./vscode;
    zathura = import ./zathura;

    catppuccin = import ./catppuccin { inherit self; };
  };
in
modules // {
  full = { imports = builtins.attrValues modules; };
}
