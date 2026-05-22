# Allow Jarne tools to pair with keyboard from browser.
{
  flake.modules.nixos.jarne = {
    services.udev.extraRules = ''
      SUBSYSTEM=="hidraw", KERNELS=="*:4C4A:0001.*", MODE="0666"
    '';
  };
}
