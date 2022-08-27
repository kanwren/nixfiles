{ nixpkgs }:

{
  oh-my-zsh = import ./oh-my-zsh.nix;
  starship = import (nixpkgs + "/nixos/modules/programs/starship.nix");
}
