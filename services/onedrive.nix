# Service to run OneDrive sync
# Set up and authorize it by running `onedrive`
{ pkgs, ... }:

let
  onedrive = pkgs.stdenv.mkDerivation {
    name = "onedrive";
    src = pkgs.fetchFromGitHub {
      owner = "abraunegg";
      repo = "onedrive";
      rev = "2a7e48b823cca934b73f691490c7344c81362f14";
      sha256 = "1d7biv32dnx70hwfh4rmfvjpfhzhb1p89nnqr9h8kvwkd9s4f9m6";
    };
    nativeBuildInputs = with pkgs; [ pkgconfig ];
    buildInputs = with pkgs; [ dmd curl sqlite ];
    installPhase = ''
      mkdir -p "$out/bin"
      cp onedrive "$out/bin"
    '';
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
    # wantedBy = [ "default.target" ];

    description = "OneDrive Free Client";
    documentation = [ "https://github.com/abraunegg/onedrive" ];
  };
}
