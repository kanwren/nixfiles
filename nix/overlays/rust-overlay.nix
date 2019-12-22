# Use Mozilla's bleeding-edge rustc/cargo overlay, but put the results in
# pkgs.rust instead, moving rustLib to a more sane location.
self: super:

let
  # nixpkgs-mozilla = builtins.fetchTarball "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz";
  nixpkgs-mozilla = super.fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "09576806ab6ac6992f6797ecb5a5cb4b3097fba1";
    sha256 = "1zm7011d6qhvp4lib56i16i99am9gy0sd2y3zl7six8r7wh2ihn3";
  };
  rust-overlay = import "${nixpkgs-mozilla}/rust-overlay.nix";
  result = rust-overlay self super;
in {
  rust = super.rust // {
    inherit (result) latest rustChannelOf rustChannelOfTargets rustChannels;
    inherit (result.lib) rustLib;
  };
}
