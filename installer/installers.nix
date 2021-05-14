{ nixos-generators
, nixpkgs
, lib
, system
}:

let
  formats = lib.pipe "${nixos-generators}/formats" [
    builtins.readDir
    (lib.filterAttrs (_: v: v == "regular"))
    builtins.attrNames
    (builtins.filter (lib.hasSuffix ".nix"))
    (builtins.map (lib.removeSuffix ".nix"))
  ];
  mkInstaller = format:
    let
      conf = import "${nixos-generators}/nixos-generate.nix" {
        inherit nixpkgs system;
        configuration = ./configuration.nix;
        formatConfig = "${nixos-generators}/formats/${format}.nix";
      };
    in
    conf.config.system.build.${conf.config.formatAttr};
in
lib.listToAttrs (builtins.map (f: { name = f; value = mkInstaller f; }) formats)
