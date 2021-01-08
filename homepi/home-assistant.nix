{ nix-cron
}:

{ config, pkgs, lib, ... }:

let
  subdomain = "rarer";
  url = "${subdomain}.duckdns.org";
  inherit (nix-cron.lib) cron;
in

{
  sops.secrets.duck-dns-token = {};

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.home-assistant = {
    enable = true;

    configDir = "/var/lib/hass";

    port = 8123;
    openFirewall = true;

    package = (pkgs.home-assistant.override {
      extraPackages = ps: with ps; [
        gtts
      ];
      extraComponents = [
        # Required for default config
        "cloud"
        "tts"
        "zeroconf"
        "ssdp"
        "mobile_app"
        "default_config"

        "zwave"
        "google_assistant"
      ];
    }).overridePythonAttrs {
      doCheck = false;
    };
  };

  security.acme = {
    email = "nprindle18@gmail.com";
    acceptTerms = true;
  };

  services.nginx = {
    enable = true;
    virtualHosts.${url} = {
      enableACME = true;
      forceSSL = true;
      extraConfig = ''
        proxy_buffering off;
      '';
      locations."/".extraConfig = ''
        proxy_pass http://127.0.0.1:${builtins.toString config.services.home-assistant.port};
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };

  services.cron = {
    enable = true;
    systemCronJobs =
      let
        updateDuckIp = pkgs.writeShellScript "update-duck-ip" ''
          token="$(</run/secrets/duck-dns-token)"
          url="https://www.duckdns.org/update?domains=${subdomain}&token=$token&verbose=true&ip="
          echo url="$url" | curl -k -o /tmp/duck.log -K - >/dev/null 2>&1
        '';
        updateJob = cron.systemJob {
          time = { minute = cron.every 5; };
          user = "root";
          commandFile = updateDuckIp;
        };
      in [ updateJob ];
  };
}

