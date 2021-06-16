final: prev:

{
  asciiquarium = prev.asciiquarium.overrideAttrs (old: {
    fixupPhase = (old.fixupPhase or "") + ''
      pushd "$out"; patch -p1 < ${./nicer-sharks.patch}; popd
    '';
  });
}
