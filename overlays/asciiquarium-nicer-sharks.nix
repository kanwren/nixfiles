final: prev:

{
  asciiquarium = prev.asciiquarium.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./patches/nicer-sharks.patch ];
  });
}
