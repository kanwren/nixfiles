{ pkgs, lib, config, system, self, inputs, ... }:

let
  scripts = { };
in
{
  home.packages = builtins.attrValues scripts;
}

