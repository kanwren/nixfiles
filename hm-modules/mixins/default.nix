{ self }:

let
  modules = {
    bash = import ./bash;
    btop = import ./btop { inherit self; };
    cava = import ./cava { inherit self; };
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
    kitty = import ./kitty { inherit self; };
    nix = import ./nix;
    rofi = import ./rofi;
    rust = import ./rust;
    spotify = import ./spotify;
    tmux = import ./tmux { inherit self; };
    vscode = import ./vscode;
    zathura = import ./zathura { inherit self; };
  };
in
modules // {
  full = { imports = builtins.attrValues modules; };
}
