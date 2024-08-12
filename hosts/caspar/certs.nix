{ pkgs, ... }:

let
  certFile = "/etc/ssl/certs/ca-bundle.crt";
in
{
  security.pki.installCACerts = false;
  environment.variables.NIX_SSL_CERT_FILE = certFile;
  system.activationScripts.extraActivation.text = ''
    printf 'Generating ${certFile}...\n'

    declare cert_tmpfile="$(mktemp /tmp/cert-XXXXXXXXXX.pem)"

    declare current_cert=""
    declare purpose=""
    declare fingerprint=""
    declare -A certs=()

    declare -i accepted=0
    declare -i expired=0
    declare -i nonroot=0
    declare -i errored=0
    declare -i unverified=0

    while read -r line; do
      # Starting a new cert
      [[ $line =~ ^-----BEGIN\ CERTIFICATE-----$ ]] && current_cert=""

      # Continue reading until done with the current cert
      current_cert+="$line"$'\n'
      [[ $line =~ ^-----END\ CERTIFICATE-----$ ]] || continue

      # Add by fingerprint
      fingerprint="$(/usr/bin/openssl x509 -inform pem -fingerprint -sha256 -noout <<< "$current_cert")" || { ((++errored)) || true; continue; }
      certs["$fingerprint"]="$current_cert"
      ((++accepted)) || true
    done < "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"

    while read -r line; do
      [[ $line =~ ^-----BEGIN\ CERTIFICATE-----$ ]] && current_cert=""
      current_cert+="$line"$'\n'
      [[ $line =~ ^-----END\ CERTIFICATE-----$ ]] || continue

      # Verify not expired
      /usr/bin/openssl x509 -inform pem -checkend 0 -noout <<< "$current_cert" || { ((++expired)) || true; continue; }

      # Verify certificate is an SSL root
      purpose="$(/usr/bin/openssl x509 -inform pem -purpose -noout <<< "$current_cert")" || continue
      [[ $purpose =~ SSL\ server\ CA\ :\ Yes ]] || { ((++nonroot)) || true; continue; }

      # Verify certificate is trusted
      printf '%s' "$current_cert" > "$cert_tmpfile"
      /usr/bin/security verify-cert -l -L -c "$cert_tmpfile" -p ssl -R offline >/dev/null 2>/dev/null || { ((++untrusted)) || true; continue; }

      fingerprint="$(/usr/bin/openssl x509 -inform pem -fingerprint -sha256 -noout <<< "$current_cert")" || { ((++errored)) || true; continue; }
      certs["$fingerprint"]="$current_cert"
      ((++accepted)) || true
    done < <(
      /usr/bin/security find-certificate -a -p /System/Library/Keychains/SystemRootCertificates.keychain
      /usr/bin/security find-certificate -a -p /Library/Keychains/System.keychain
    )

    rm -f "$cert_tmpfile"

    mkdir -p "$(dirname ${certFile})"
    printf '%s\n' "''${certs[@]}" > ${certFile}

    printf 'Generated ${certFile}; %d accepted, %d untrusted, %d expired, %d not root, %d errored\n' "$accepted" "$untrusted" "$expired" "$nonroot" "$errored"
  '';
}
