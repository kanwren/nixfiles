let
  scripts = { };
in
{
  home.packages = builtins.attrValues scripts;
}

