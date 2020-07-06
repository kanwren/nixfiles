{ lib }:

# Various helper functions
rec {
  # Get all subdirectories in a directory
  getDirs = dir:
    builtins.map (x: dir + "/${x}")
    (builtins.attrNames
    (lib.filterAttrs (_: type: type == "directory")
    (builtins.readDir dir)));

  # Get all files in a directory
  getFiles = getFilesWith (_: _: true);

  # Get all files in a directory matching a predicate
  getFilesWith = pred: dir:
    builtins.map (x: dir + "/${x}")
    (builtins.attrNames
    (lib.filterAttrs (name: type:
      pred name type && (type == "regular" || type == "symlink"))
    (builtins.readDir dir)));
}
