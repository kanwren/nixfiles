{
  inputs,
  ...
}:

let
  enableRevisionTracking = false;
  module = {
    system.configurationRevision =
      if enableRevisionTracking then inputs.self.rev or inputs.self.dirtyRev or null else null;
  };
in
{
  flake.modules.nixos.base = module;
  flake.modules.darwin.base = module;
}
