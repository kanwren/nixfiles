{ pkgs, lib, ... }:

{
  users.users.nprin = {
    initialPassword = "setup";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "audio"
      "video"
      "networkmanager"
      "docker"
      "vboxusers"
      "libvirtd"
      "wireshark"
    ];
    createHome = true;
    shell = pkgs.zsh;

    openssh.authorizedKeys.keys =
      let
        keyfile = builtins.fetchurl {
          url = "https://github.com/nprindle.keys";
          sha256 = "0zkrfyn9g1cgzr5likz0s0cq5qil78n53154yha0ha5z4dznkh87";
        };
      in
      builtins.filter (x: x != "") (lib.splitString "\n" (builtins.readFile keyfile));
  };

  nix.trustedUsers = [ "nprin" ];
}

# Note: to generate an initialHashedPassword, use 'mkpasswd -m sha-512 -s'
