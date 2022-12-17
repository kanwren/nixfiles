{ config
, ...
}:

let
  subdomain = "rarer";
  url = "${subdomain}.duckdns.org";
  hassPort = 8123; # the default hass port
in

{
  sops.secrets = {
    duck-dns-token = { };
    "google-home-service-account.json" = {
      format = "binary";
      sopsFile = ./secrets/google-home-service-account.keytab;
      path = "/var/lib/hass/service-account.json";

      inherit (config.users.extraUsers.hass) group;
      owner = config.users.extraUsers.hass.name;
    };
  };
  users.extraUsers.hass.extraGroups = [ "keys" ];

  networking.firewall.allowedTCPPorts = [ 80 443 hassPort ];

  services.home-assistant = {
    enable = true;
    config = null;
    extraPackages = ps: with ps; [ gtts ];
    extraComponents = [
      # Required for default config
      "cloud"
      "tts"
      "zeroconf"
      "ssdp"
      "mobile_app"
      "default_config"

      "zwave_js"
      "google_assistant"
    ];
  };

  security.acme = {
    defaults.email = "nicole@wren.systems";
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
        proxy_pass http://127.0.0.1:${builtins.toString hassPort};
        proxy_set_header Host $host;
        proxy_redirect http:// https://;
        proxy_http_version 1.1;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection $connection_upgrade;
      '';
    };
  };

  services.duckdns = {
    enable = true;
    inherit subdomain;
    tokenPath = "/run/secrets/duck-dns-token";
    onCalendar = "*-*-* *:00,30:00"; # Update twice per hour
  };
}

