{ pkgs, ... }:

{
  home.sessionPath = [
    "$HOME/.cargo/bin"
  ];

  home.packages = with pkgs; [
    rustup

    # cargo extensions
    cargo-cache
    cargo-download
    cargo-feature
    cargo-watch
    cargo-wipe
  ];
}

