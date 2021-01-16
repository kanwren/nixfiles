{ lib }:

with lib;

{
  required = type: {
    required = true;
    inherit type;
  };

  optional = type: {
    required = false;
    inherit type;
  };

  object = allowExtraAttrs: ts:
    let
      noExtraAttrs = as: builtins.removeAttrs as (builtins.attrNames ts) == {};
      validAttr = as: k:
        if as ? ${k}
        then ts.${k}.type.check as.${k}
        else !ts.${k}.required;
      allValidAttrs = as: builtins.all (validAttr as) (builtins.attrNames ts);
      description = "attribute set of {${concatMapStringsSep " " (k: "${k} = ${ts.${k}.description};") (builtins.attrNames ts)}}";
    in types.addCheck types.attrs (as:
      allValidAttrs as && (allowExtraAttrs || noExtraAttrs as)
    ) // { inherit description; };
}
