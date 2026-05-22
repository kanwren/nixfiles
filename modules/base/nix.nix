{
  inputs,
  ...
}:

{
  flake.modules = {
    nixos.base = {
      nix = {
        channel.enable = false;

        settings = {
          experimental-features = [
            "nix-command"
            "flakes"
            "ca-derivations"
          ];
          trusted-users = [ "root" ];
          keep-outputs = true;
          keep-derivations = true;
          substituters = [ "https://cache.lix.systems" ];
          trusted-public-keys = [ "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o=" ];
        };
      };

      nixpkgs.config.allowUnfree = true;

      programs.nix-ld.enable = true;
    };

    darwin.base = {
      ids.gids.nixbld = 350;

      nix = {
        enable = true;

        registry.nixpkgs.to = {
          type = "path";
          path = inputs.nixpkgs.outPath;
        };
        nixPath = inputs.nixpkgs.lib.mkForce [ "nixpkgs=flake:nixpkgs" ];

        extraOptions = ''
          experimental-features = nix-command flakes ca-derivations
          keep-outputs = true
          keep-derivations = true
        '';
      };
    };
  };
}
