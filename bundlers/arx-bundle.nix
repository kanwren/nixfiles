# nix-bundle shim to bundle programs with names other than the default
# See https://github.com/matthewbauer/nix-bundle/issues/74

{ nixpkgs, nix-bundle }:

{ program, system }:

let
  pkgs = nixpkgs.legacyPackages.${system};
  bundle = import nix-bundle { nixpkgs = pkgs; };
  envProg = builtins.getEnv "PROGRAM";
  prog =
    if envProg == "" then
      builtins.trace "Warning: PROGRAM not set; defaulting to '${program}'. Did you forget to set PROGRAM or --impure?" program
    else
      "${builtins.dirOf program}/${envProg}";
  script = pkgs.writeScript "startup" ''
    #!/bin/sh
    .${bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -- "${prog}" "$@"
  '';
in
bundle.makebootstrap {
  targets = [ script ];
  startup = ".${builtins.unsafeDiscardStringContext script} '\"$@\"'";
}
