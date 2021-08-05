{ pkgs
, lib
, config
, custom
, inputs
, ...
}:

{
  imports = [
    ./nprin
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
