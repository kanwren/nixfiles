{
  inputs,
  rev,
}: let
  inherit (inputs) nixpkgs nix-darwin;
in {
  mkNixosSystem = {
    hostname,
    systemModules ? [],
    hmModules ? [],
    overlays ? [],
  }:
    nixpkgs.lib.nixosSystem {
      modules = let
        base = {
          networking.hostName = hostname;
          system.configurationRevision = rev;
          nix.settings.experimental-features = ["nix-command" "flakes" "ca-derivations"];
          nix.channel.enable = false;
          nixpkgs.overlays = overlays;
          home-manager.sharedModules = hmModules;
        };
      in
        [base] ++ systemModules;
    };

  mkDarwinSystem = {
    hostname,
    system,
    systemModules ? [],
    hmModules ? [],
    overlays ? [],
  }:
    nix-darwin.lib.darwinSystem {
      inherit system;
      modules = let
        base = {
          networking = {
            computerName = hostname;
            hostName = "${hostname}.local";
            localHostName = hostname;
          };
          system.configurationRevision = rev;
          nix = {
            enable = true;
            registry.nixpkgs.to = {
              type = "path";
              path = nixpkgs.outPath;
            };
            nixPath = nixpkgs.lib.mkForce ["nixpkgs=flake:nixpkgs"];
            extraOptions = ''
              experimental-features = nix-command flakes ca-derivations
              keep-outputs = true
              keep-derivations = true
            '';
            settings = {
              sandbox = true;
              extra-sandbox-paths = ["/private/tmp" "/private/var/tmp" "/private/etc" "/usr/bin/env"];
            };
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
          nixpkgs.overlays = overlays;
          home-manager.sharedModules = hmModules;
        };
      in
        [base] ++ systemModules;
    };
}
