{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
      keep-outputs = true
      keep-derivations = true
    '';
    useSandbox = true;
    sandboxPaths = [ "/private/tmp" "/private/var/tmp" "/usr/bin/env" ];
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
