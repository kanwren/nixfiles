{
  flake.modules = {
    nixos.shell = {
      programs.fish.enable = true;
    };

    darwin.shell = {
      programs.fish.enable = true;
    };

    homeManager.shell =
      { pkgs, ... }:
      {
        home = {
          shell.enableFishIntegration = true;

          packages = [
            pkgs.fishPlugins.foreign-env
            (pkgs.fishPlugins.fzf-fish.overrideAttrs { doCheck = false; })
            pkgs.wrenpkgs.wd-fish
          ];
        };

        programs.fish = {
          enable = true;

          interactiveShellInit = /* fish */ ''
            fish_vi_key_bindings
          '';

          functions = {
            fish_prompt = {
              description = "a minimal prompt";
              body = /* fish */ ''
                set --local last_status $status
                if set -q SSH_TTY
                  prompt_login
                  printf ' '
                end
                test $last_status = 0; and set_color --bold green; or set_color --bold red
                printf '$'
                set_color normal
                printf ' '
              '';
            };

            fish_mode_prompt = {
              description = "no mode prompt";
              body = "";
            };

            fish_greeting = {
              description = "no greeting";
              body = "";
            };

            # Misc shell utilities

            "yield" = {
              description = "Yield the arguments";
              body = /* fish */ ''
                if test (count $argv) -gt 0
                  printf '%s\0' $argv | string split0
                end
              '';
            };

            dump = {
              description = "Quote each argument for fish and present the results like a command";
              body = /* fish */ ''
                string join -- ' ' (string escape --style=script -- $argv)
                return 0
              '';
            };

            _expand_which = {
              description = "Expand =foo to the path to foo";
              body = /* fish */ ''
                if test (string sub --end 2 $argv[1]) = '=='
                  realpath (command --search (string sub --start 3 $argv[1]))
                else
                  command --search (string sub --start 2 $argv[1])
                end
              '';
            };

            _expand_last_command = {
              description = "Expand the last command";
              body = /* fish */ ''
                echo $history[1]
              '';
            };
          };

          shellAbbrs = {
            "expand_seq_abbr" = {
              position = "anywhere";
              function = /* fish */ "_expand_seq";
              regex = ''.*\{\d+\.\.\d+\}.*'';
            };
            "expand_which_abbr" = {
              position = "anywhere";
              function = /* fish */ "_expand_which";
              regex = ''==?\S+'';
            };
            "!!" = {
              position = "anywhere";
              function = /* fish */ "_expand_last_command";
            };
            "find1" = {
              position = "command";
              setCursor = "%";
              expansion = /* fish */ "find % -mindepth 1 -maxdepth 1";
            };
            "-sh" = {
              command = "find";
              position = "anywhere";
              setCursor = "%";
              expansion = /* fish */ "-exec sh -c 'x=\"$1\"; %' -- {} ';'";
            };
          };
        };
      };
  };
}
