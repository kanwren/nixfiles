{ nixpkgs }:

{
  oh-my-zsh = import ./oh-my-zsh.nix;
  starship = import ./starship.nix;
}
