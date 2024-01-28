{ self }:

{
  ollama = import ./ollama.nix;
  pueue = import ./pueue.nix;
}
