{
  flake.modules.nixos.gaming = {
    programs.steam.enable = true;
  };

  flake.modules.homeManager.gaming =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.dolphin-emu # emulator
        pkgs.joycond-cemuhook # expose joycons as joint device over udp for dolphin-emu
      ];
    };
}
