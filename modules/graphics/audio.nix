{
  flake.modules.nixos.graphics =
    { pkgs, ... }:
    {
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      environment.systemPackages = [
        pkgs.pulseaudio
        pkgs.pwvucontrol
      ];
    };
}
