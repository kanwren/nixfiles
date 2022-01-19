{ ... }:

{
  networking = {
    hostName = "hecate";
    interfaces = {
      enp2s0.useDHCP = true;
      wlp3s0.useDHCP = true;
    };
  };
}
