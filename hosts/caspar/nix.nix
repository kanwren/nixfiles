{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
      keep-outputs = true
      keep-derivations = true
    '';
    settings = {
      sandbox = true;
      extra-sandbox-paths = [ "/private/tmp" "/private/var/tmp" "/usr/bin/env" ];
      substituters = [ "https://nprindle.cachix.org" ];
      trusted-public-keys = [ "nprindle.cachix.org-1:hRW0f/n4hCZZzTzYJO9olDjJ+8MB4VpknEGpiVCxpWo=" ];
      trusted-users = [ "nprindle" ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  services = {
    nix-daemon.enable = true;
    activate-system.enable = true;
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
