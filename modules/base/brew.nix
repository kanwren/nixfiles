{
  flake.modules.darwin.base =
    { config, lib, ... }:
    let
      homebrewPrefix = builtins.toString config.homebrew.prefix;
    in
    {
      homebrew = {
        enable = true;
      };

      environment.variables = {
        HOMEBREW_NO_ANALYTICS = "1";
        HOMEBREW_PREFIX = homebrewPrefix;
        HOMEBREW_CELLAR = "${homebrewPrefix}/Cellar";
        HOMEBREW_REPOSITORY = homebrewPrefix;
      };

      # TODO: handle other shells
      programs.fish.loginShellInit = ''
        fish_add_path --move --prepend --path \
            "/usr/local/bin" \
            "/usr/local/sbin" \
            "/opt/local/bin"

        # set up extra homebrew variables
        if command --search ${homebrewPrefix}/bin/brew >/dev/null 2>&1
            # Used for C pre-processor/#include. Confirm paths with `clang -x c -v -E /dev/null`
            not set -q CPATH; and set CPATH ""
            set --global --export CPATH ${homebrewPrefix}/include:"$CPATH"

            # Used by linker. Confirm paths with `clang -Xlinker -v`
            not set -q LIBRARY_PATH; and set LIBRARY_PATH ""
            set --global --export LIBRARY_PATH ${homebrewPrefix}/lib:"$LIBRARY_PATH"

            not set -q MANPATH; and set MANPATH ""
            set --global --export MANPATH ${homebrewPrefix}/share/man:"$MANPATH"

            not set -q INFOPATH; and set INFOPATH ""
            set --global --export INFOPATH ${homebrewPrefix}/share/info:"$INFOPATH"

            fish_add_path --move --prepend --path \
                "${homebrewPrefix}/bin" \
                "${homebrewPrefix}/sbin"
        end

        # give NixOS paths priority over brew and system paths
        fish_add_path --move --prepend --path ${
          lib.strings.concatMapStringsSep " " (p: builtins.toJSON "${p}/bin") config.environment.profiles
        }

        set fish_user_paths $fish_user_paths
      '';
    };
}
