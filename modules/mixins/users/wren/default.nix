self:

let
  modules = {
    base = import ./base;
    home = import ./home self;
  };
in

modules // {
  full = {
    imports = builtins.attrValues modules;
  };
}
