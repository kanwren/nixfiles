{
  config,
  pkgs,
  ...
}: let
  intToString = n: assert builtins.isInt n; builtins.toString n;

  nasPhotosMount = "/mnt/immich";
  nasHost = "gwenas";
  nasShare = "Wren/Photos";

  immichUser = config.services.immich.user;
  immichGroup = config.services.immich.group;
in {
  environment.systemPackages = [
    pkgs.immich-cli
    pkgs.cifs-utils
  ];

  services.immich = {
    enable = true;
    accelerationDevices = null;
    mediaLocation = nasPhotosMount;
    host = "127.0.0.1";
    machine-learning.environment = {
      MACHINE_LEARNING_PRELOAD__CLIP__TEXTUAL = "ViT-B-32__openai";
      MACHINE_LEARNING_PRELOAD__CLIP__VISUAL = "ViT-B-32__openai";
      MACHINE_LEARNING_PRELOAD__FACIAL_RECOGNITION__RECOGNITION = "buffalo_l";
      MACHINE_LEARNING_PRELOAD__FACIAL_RECOGNITION__DETECTION = "buffalo_l";
      HF_XET_CACHE = "/var/cache/immich/huggingface-xet"; # TODO(wren): https://github.com/NixOS/nixpkgs/issues/418799
    };
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

  # Expose immich to tailnet via Caddy
  services.tscaddy = {
    enable = true;
    nodes.immich = {
      host = "https://immich.swallow-chickadee.ts.net";
      target = "http://127.0.0.1:${intToString config.services.immich.port}";
      authKeyFile = config.sops.secrets."caddy/ts-authkey-immich".path;
      dependencies = ["immich-server.service"];
    };
  };

  sops.secrets = {
    "immich/nas-credentials" = {
      sopsFile = ../secrets/immich/nas-credentials.txt;
      format = "binary";
      mode = "0440";
    };

    "caddy/ts-authkey-immich" = {
      sopsFile = ../secrets/caddy/ts-authkey-immich.txt;
      format = "binary";
      mode = "0440";
      owner = config.services.caddy.user;
      group = config.services.caddy.group;
    };
  };
}
