{
  imports = [
    ./wren.nix
  ];

  users = {
    mutableUsers = true;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'
