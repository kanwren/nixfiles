final: prev:

{
  asciiquarium = prev.asciiquarium.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./nicer-sharks.patch ];
  });
}
