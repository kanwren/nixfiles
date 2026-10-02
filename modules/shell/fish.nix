{
  flake.modules = {
    nixos.shell = {
      programs.fish.enable = true;
    };

    darwin.shell = {
      programs.fish.enable = true;
      nixpkgs.config.problems.handlers."fzf.fish".broken = "warn";
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

            yield = {
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

            sourceenv = {
              description = "Source a .env file";
              body = /* fish */ ''
                set --local result 0
                set --local exported_vars

                for file in $argv
                  if not test -f $file
                    printf 'failed to load file: %s\n' $file >&2
                    set result 1
                  end

                  while read --local line
                    string match --quiet --regex '^\\s*(#.*)?$' $line
                    and continue

                    set --local item (string split --max 1 '=' $line)
                    set --global --export $item[1] (string unescape --style=script -- $item[2])
                    set --append exported_vars $item[1]
                  end < $file

                  printf 'exported %s\n' (string join ' ' -- $exported_vars)
                end

                return $result
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
