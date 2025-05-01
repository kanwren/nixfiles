{
  imports = [
    ./caches.nix
  ];

  system.autoUpgrade.enable = false;

  nix = {
    settings = {
      experimental-features = [ "ca-derivations" ];
      keep-outputs = true;
      keep-derivations = true;
      trusted-users = [ "root" ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  programs.nix-ld.enable = true;
}
