{
  config,
  pkgs,
  ...
}:
{
  # ACME-DNS for wildcard TLS cert for custom domain:
  #
  # To register, run the following for the secret contents:
  # ```
  # curl -X POST https://auth.acme-dns.io/register | jq '.server_url = "https://auth.acme-dns.io"'
  # ```
  #
  # Then CNAME _acme-challenge to the contents of '.fulldomain'
  sops.secrets."caddy/acmedns-creds-cule_gay" = {
    sopsFile = ../secrets/caddy/acmedns-creds-cule_gay.json.txt;
    format = "binary";
    mode = "0440";
    owner = config.services.caddy.user;
    group = config.services.caddy.group;
  };

  services.caddy = {
    package = pkgs.caddy-with-plugins;

    # Standard Caddy advice for using wildcard certificates is to only have a
    # virtual host for the wildcard, and then match the actual domain to route
    # requests to handlers. However, this assumes that all of these domains are
    # on the same listener in the first place. In our case, each of our domains
    # are bound to separate Tailscale devices; we need to skip automation and
    # reuse the wildcard cert. We can use Caddy 2.10's `auto_https prefer_wildcard`
    # to disable cert automation for the virtual hosts for which we already have
    # wildcard certs.
    globalConfig = ''
      auto_https prefer_wildcard disable_redirects
    '';

    # Dummy host to establish the management of the wildcard cert.
    virtualHosts."https://*.cule.gay:26676".extraConfig = ''
      tls {
        dns acmedns ${config.sops.secrets."caddy/acmedns-creds-cule_gay".path}
      }
      abort
    '';
  };
}
