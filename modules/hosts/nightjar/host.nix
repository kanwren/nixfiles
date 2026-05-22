{
  inputs,
  config,
  ...
}:

{
  flake.darwinConfigurations.nightjar = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";

    modules = [
      {
        networking.computerName = "nightjar";
        primaryUser = "wrenn";

        # These files are provisioned out-of-band on this machine.
        environment.etc."bashrc".target = "bashrc_nix"; # Manually patch to source bashrc_nix
        home-manager.sharedModules = [ { home.file.".bashrc".force = true; } ];
      }

      config.flake.modules.darwin.base
      config.flake.modules.darwin.shell
      config.flake.modules.darwin.graphics
      config.flake.modules.darwin.a11y
      config.flake.modules.darwin."users/wrenn"
    ];
  };
}
