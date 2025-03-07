{ config, pkgs, ... }:

let
  intToString = n: assert builtins.isInt n; builtins.toString n;

  nasPhotosMount = "/mnt/immich";
  nasHost = "gwenas.ghost-bowfin.ts.net";
  nasShare = "Wren/Photos";

  immichUser = config.services.immich.user;
  immichGroup = config.services.immich.group;
in
{
  environment.systemPackages = [
    pkgs.immich-cli
    pkgs.cifs-utils
  ];

  services.immich = {
    enable = true;
    # accelerationDevices = null;
    mediaLocation = nasPhotosMount;
    host = "127.0.0.1";
  };
  systemd.services.immich-server.serviceConfig.Restart = "on-failure";

  # Expose immich to tailnet via tsnsrv
  sops.secrets."immich/tsnsrv-ts-authkey" = {
    sopsFile = ../secrets/tsnsrv-immich.txt;
    format = "binary";
    mode = "0440";
    owner = immichUser;
    group = immichGroup;
  };
  services.tsnsrv = {
    enable = true;
    services.immich = {
      urlParts = {
        protocol = "http";
        host = "127.0.0.1";
        port = config.services.immich.port;
      };
      authKeyPath = config.sops.secrets."immich/tsnsrv-ts-authkey".path;
      supplementalGroups = [ immichGroup ];
    };
  };
  systemd.services.tsnsrv-immich = {
    wants = [ "tailscaled.service" "sops-nix.service" "immich-server.service" ];
    after = [ "tailscaled.service" "sops-nix.service" "immich-server.service" ];
    serviceConfig.Restart = "on-failure";
  };

  # Mount NAS over CIFS as the backing image store for immich
  users = {
    # Give the immich user/group an explicit uid/gid so we can reference it in
    # mount ownership mapping below
    users.${immichUser}.uid = 989;
    groups.${immichGroup}.gid = 983;
  };
  fileSystems."${nasPhotosMount}" = {
    device = "//${nasHost}/${nasShare}";
    fsType = "cifs";
    options = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=60"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
      "uid=${intToString config.users.users.${immichUser}.uid}"
      "gid=${intToString config.users.groups.${immichGroup}.gid}"
      "credentials=${config.sops.secrets."immich/nas-credentials".path}"
      # credentials format:
      # ```
      # username=...
      # password=...
      # ```
    ];
  };
  sops.secrets."immich/nas-credentials" = {
    sopsFile = ../secrets/nas-credentials.txt;
    format = "binary";
    mode = "0440";
  };
  systemd.services.immich-server = {
    wants = [ "mnt-immich.mount" ];
    after = [ "mnt-immich.mount" ];
  };
}
