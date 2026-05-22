{
  flake.modules.darwin.base = {
    nix.settings = {
      sandbox = true;
      extra-sandbox-paths = [
        "/private/tmp"
        "/private/var/tmp"
        "/private/etc"
        "/usr/bin/env"
      ];
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
  };
}
