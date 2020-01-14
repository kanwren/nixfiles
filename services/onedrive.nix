# Service to run OneDrive sync
# Set up and authorize it by running `onedrive`
{ pkgs, ... }:

let
  onedrive = pkgs.stdenv.mkDerivation {
    name = "onedrive";
    src = pkgs.fetchFromGitHub {
      owner = "skilion";
      repo = "onedrive";
      rev = "945251f7f2e95ae85001efb6eab85d6176bac75e";
      sha256 = "16iajb61b09gdqly85h6h7ap385ihk0az3mimkj277yc08rv68d0";
    };
    buildInputs = with pkgs; [ dmd curl sqlite ];
    installPhase = ''
      mkdir -p "$out/bin"
      cp onedrive "$out/bin"
    '';
    # This uses the .git directory for version information, so we inline the
    # version instead
    patches = [ ./inline-onedrive-version-v1.3.3.patch ];
  };
in
{
  environment.systemPackages = [ onedrive ];
  # Service configuration is according to generated onedrive.service
  systemd.user.services.onedrive-sync = {
    script = "${onedrive}/bin/onedrive --monitor";
    serviceConfig = {
      Restart = "no";
    };
    wantedBy = [ "default.target" ];

    description = "OneDrive Free Client";
    documentation = [ "https://github.com/skilion/onedrive" ];
  };
}
