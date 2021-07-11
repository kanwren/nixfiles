{ ... }:

{
  services.logind = {
    extraConfig = ''
      IdleAction=ignore
    '';
  };
}
