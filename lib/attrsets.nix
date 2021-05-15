{ lib }:

rec {
  # Return a list of all values in an attrset recursively, expanding every
  # non-derivation attrset
  recursiveValues =
    let
      strictIsAttrs = a: builtins.isAttrs a && !lib.isDerivation a;
      go = as:
        if !strictIsAttrs as
        then [ as ]
        else builtins.concatMap go (builtins.attrValues as);
    in
    go;

  # Return a list of all derivations in an attrset recursively, expanding every
  # non-derivation attrset
  recursiveDerivations = as:
    builtins.filter lib.isDerivation (recursiveValues as);

  # Combine a list of attribute sets with a right-biased merge. If two attribute
  # sets share a key, their values are merged recursively.
  recursiveMergeAttrs = lib.foldl' lib.recursiveUpdate { };
}
