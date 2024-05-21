{ pkgs, ... }:

{
  programs.gh = {
    enable = true;

    extensions = [
      pkgs.gh-copilot
      pkgs.gh-ost
    ];

    settings = {
      git_protocol = "https";
      prompt = "enabled";
      aliases = {
        co = "pr checkout";
      };
    };
  };
}
