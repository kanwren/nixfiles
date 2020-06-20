{ pkgs, ... }:

{
  imports = [ ./nushell.nix ];

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

