{
  lib,
  inputs,
  ...
}:

let
  flattenPackages =
    path: value:
    if lib.isDerivation value then
      { ${lib.concatStringsSep "/" path} = value; }
    else if lib.isAttrs value then
      lib.concatMapAttrs (key: flattenPackages (path ++ [ key ])) value
    else
      { };
in

{
  flake.overlays.default =
    final: prev:
    let
      scope = prev.lib.makeScope final.newScope (_: {
        inherit inputs;
      });
    in
    {
      local = prev.lib.filesystem.packagesFromDirectoryRecursive {
        directory = ../../pkgs/by-name;
        inherit (scope) newScope callPackage;
      };
    };

  perSystem =
    { pkgs, ... }:
    rec {
      legacyPackages = lib.removeAttrs pkgs.local [ "packages" ];

      packages = flattenPackages [ ] legacyPackages;
    };
}
