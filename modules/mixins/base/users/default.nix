{ pkgs, ... }:

{
  users = {
    mutableUsers = true;
  };
}

# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'
