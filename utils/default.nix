{ lib, ... }:

# Various helper functions
{
  lib = {
    utils = rec {

      importOr = path: default:
        if builtins.pathExists path
          then import path
          else default;

      # Access multiple names in a nested attribute set, with a default value if
      # any accesses fail
      attrChain = names: default: attrs:
        if names == []
          then attrs
          else
            let name = builtins.head names;
            in if builtins.hasAttr name attrs
              then attrChain (builtins.tail names) default (builtins.getAttr name attrs)
              else default;

      # Map key-value pairs of an attribute set
      mapAttrPairs = f: attr:
        builtins.listToAttrs
        (builtins.map (n: f { name = n; value = builtins.getAttr n attr; })
        (builtins.attrNames attr));

      # Get all subdirectories in a directory
      getDirs = dir:
        builtins.map (x: dir + "/${x}")
        (builtins.attrNames
        (lib.filterAttrs (_: type: type == "directory")
        (builtins.readDir dir)));

      # Get all files in a directory
      getFiles = dir:
        builtins.map (x: dir + "/${x}")
        (builtins.attrNames
        (lib.filterAttrs (_: type: type == "regular" || type == "symlink")
        (builtins.readDir dir)));

    };
  };
}
