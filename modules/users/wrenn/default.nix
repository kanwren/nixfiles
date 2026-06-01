{ config, ... }@toplevel:

let
  userInfo = {
    name = "Nicole Wren";
    email = "wrenn@block.xyz";
  };
in
{
  flake.modules.darwin."users/wrenn" = {
    users.users.wrenn.home = "/Users/wrenn";

    nix.settings.trusted-users = [ "wrenn" ];

    home-manager.users.wrenn = {
      imports = [ config.flake.modules.homeManager."users/wrenn" ];
    };
  };

  flake.modules.homeManager."users/wrenn" =
    { pkgs, lib, config, ... }:
    {
      imports = [
        toplevel.config.flake.modules.homeManager.aws
        toplevel.config.flake.modules.homeManager.bazel
        toplevel.config.flake.modules.homeManager.btop
        toplevel.config.flake.modules.homeManager.catppuccin
        toplevel.config.flake.modules.homeManager.git
        toplevel.config.flake.modules.homeManager.go
        toplevel.config.flake.modules.homeManager.gpg
        toplevel.config.flake.modules.homeManager.jujutsu
        toplevel.config.flake.modules.homeManager.k8s
        toplevel.config.flake.modules.homeManager.shell
        toplevel.config.flake.modules.homeManager.zellij
      ];

      home = {
        stateVersion = "24.11";

        sessionPath = [
          "$HOME/bin"
          "$HOME/.local/bin"
          "$HOME/.docker/bin"
          "$HOME/.krew/bin"
        ];

        sessionVariables = {
          EDITOR = "nvim";
          VISUAL = "nvim";
          SHELL = lib.getExe pkgs.fish;
        };

        packages = [
          pkgs.crudini
          pkgs.dive
          pkgs.dyff
          pkgs.fnm
          pkgs.grpcurl
          pkgs.httpie
          pkgs.jira-cli-go
          pkgs.shellcheck
          pkgs.skopeo
          pkgs.tz
          pkgs.wrenpkgs.frum
          pkgs.wrenpkgs.reargc
          pkgs.wrenpkgs.tfenv
        ];
      };

      programs = {
        h.codeRoot = "$HOME/Development/code";

        go.env.GOPATH = "${config.home.homeDirectory}/Development/go";

        fish.interactiveShellInit = ''
          ${lib.getExe pkgs.wrenpkgs.frum} init | source
          ${lib.getExe pkgs.fnm} env | source
        '';

        git.settings = {
          user = userInfo;
          signing = {
            signByDefault = true;
            key = "85C1D51ACE6FFEC848B78D89E6A60F3BD112FD42";
            format = "openpgp";
          };
        };

        jujutsu.settings = {
          user = userInfo;
          signing = {
            behavior = "own";
            backend = "gpg";
            key = "85C1D51ACE6FFEC848B78D89E6A60F3BD112FD42";
          };
          templates.git_push_bookmark = ''"wrenn/" ++ change_id.short()'';
        };
      };

      xdg.configFile = builtins.foldl' (acc: as: acc // as) { } [
        (lib.mapAttrs'
          (name: value: {
            name = "fish/completions/${name}.fish";
            inherit value;
          })
          {
            docker.source = "${pkgs.docker}/share/fish/vendor_completions.d/docker.fish";
            jira.source = pkgs.runCommand "jira-cli-go-completions" {
              nativeBuildInputs = [ pkgs.jira-cli-go ];
            } "jira completion fish > $out";
          }
        )
      ];
    };
}
