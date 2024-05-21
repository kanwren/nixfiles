{ self }:

{
  imports = [ self.hmModules.h ];
  programs.h.enable = true;
}
