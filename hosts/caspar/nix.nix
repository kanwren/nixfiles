{
  system.stateVersion = 5;

  ids.gids.nixbld = 30000;

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
      keep-outputs = true
      keep-derivations = true
    '';
    settings = {
      sandbox = true;
      extra-sandbox-paths = [ "/private/tmp" "/private/var/tmp" "/private/etc" "/usr/bin/env" ];
      trusted-users = [ "wrenn" ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  services = {
    nix-daemon.enable = true;
  };

  environment.etc."nix/user-sandbox.sb".text = ''
    (version 1)
    (allow default)
    (deny file-write*
          (subpath "/nix"))
    (allow file-write*
           (subpath "/nix/var/nix/gcroots/per-user")
           (subpath "/nix/var/nix/profiles/per-user"))
    (allow process-exec
          (literal "/bin/ps")
          (with no-sandbox))
  '';
}
