{ pkgs, ... }:

{
  users = {
    mutableUsers = true;
    defaultUserShell = pkgs.zsh;
  };
}

# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'
