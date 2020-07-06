{ pkgs, ... }:

let
  # Need to import like this for modules to work
  nur = import ../../common/nur.nix { inherit pkgs; };
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

