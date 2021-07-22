{ nixpkgs }:

{ program, system }:

let
  pkgs = nixpkgs.legacyPackages.${system};
  envProg = builtins.getEnv "PROGRAM";
  prog =
    if envProg == "" then
      builtins.trace "Warning: PROGRAM not set; defaulting to '${program}'. Did you forget to set PROGRAM or --impure?" program
    else
      "${builtins.dirOf program}/${envProg}";
in
pkgs.dockerTools.buildImage {
  name = "${builtins.baseNameOf program}";
  tag = "latest";
  config.Cmd = [ prog ];
}

