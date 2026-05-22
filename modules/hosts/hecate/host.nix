{
  inputs,
  config,
  ...
}:

{
  flake.nixosConfigurations.hecate = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      { networking.hostName = "hecate"; }
      config.flake.modules.nixos."hecate/disko-config"
      config.flake.modules.nixos.asus-tuf-a17

      config.flake.modules.nixos.base
      config.flake.modules.nixos.bluetooth
      config.flake.modules.nixos.graphics
      config.flake.modules.nixos.jarne
      config.flake.modules.nixos.shell
      config.flake.modules.nixos.tailscale

      config.flake.modules.nixos."users/wren"
    ];
  };
}
