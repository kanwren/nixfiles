final: prev:

{
  h = prev.h.overrideAttrs (old: {
    patches = [ ./patches/fix-h-warning.patch ];
  });
}
