{ pkgs, ... }:

let
  # Need to import like this for modules to work
  sources = import ../../nix/sources.nix;
  nur = import sources.NUR { inherit pkgs; };
in {
  imports = [ nur.repos.nprindle.hm-modules.nushell ];

  programs.nushell = {
    enable = true;
    settings = {
      edit_mode = "vi";
      key_timeout = 0;
      pivot_mode = "never";
      startup = [
        ''alias nrn [] { ${pkgs.nix}/bin/nix repl '\<nixpkgs\>' '\<nixpkgs/nixos\>' }''
      ];
    };
  };
}

