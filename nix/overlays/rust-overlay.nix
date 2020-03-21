# Use Mozilla's bleeding-edge rustc/cargo overlay, but put the results in
# pkgs.rust instead, moving rustLib to a more sane location.
self: super:

let
  # nixpkgs-mozilla = builtins.fetchTarball "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz";
  nixpkgs-mozilla = super.fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "e912ed483e980dfb4666ae0ed17845c4220e5e7c";
    sha256 = "08fvzb8w80bkkabc1iyhzd15f4sm7ra10jn32kfch5klgl0gj3j3";
  };
  rust-overlay = import "${nixpkgs-mozilla}/rust-overlay.nix";
  result = rust-overlay self super;
in {
  rust = super.rust // {
    inherit (result) latest rustChannelOf rustChannelOfTargets rustChannels;
    lib = result.lib.rustLib;
  };
}
