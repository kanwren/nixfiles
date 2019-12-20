# Compile sudo with insults
self: super:
{
  sudo = super.sudo.override {
    withInsults = true;
  };
}
