{ pkgs, config, lib, ... }:

let
  cfg = config.mixins.jujutsu;

  jj-helpers =
    let
      helpersScript = pkgs.replaceVars ./helpers.bash {
        jj = "${pkgs.jujutsu}/bin/jj";
        jq = "${pkgs.jq}/bin/jq";
      };
      helpersBash = pkgs.writers.writeBashBin "jj" helpersScript;
      helpersArgcBuilt = pkgs.runCommandNoCC "argc-build-jj-helpers" { nativeBuildInputs = [ pkgs.coreutils pkgs.argc ]; } ''
        orig=${lib.strings.escapeShellArg (lib.getExe helpersBash)}
        argc --argc-build "$orig" "$out/bin/$(basename "$orig")"
      '';
    in
    "${helpersArgcBuilt}/bin/jj";
in
{
  options.mixins.jujutsu.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the jujutsu mixin";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.difftastic
    ];

    programs.jujutsu = {
      enable = true;

      settings = {
        user.name = "Nicole Wren";

        ui = {
          allow-filesets = true;
          pager = "less -RF";
          paginate = "auto";
          default-command = "worklog";
          diff-editor = ":builtin";
          diff.tool = "difftastic";
          merge-editor = "vimdiff";
        };

        merge-tools = {
          vimdiff = {
            program = "nvim";
            # similar to the default, but opens files in a different order to
            # preserve commands like `1do`.
            merge-args = [ "-d" "-M" "$left" "$base" "$right" "$output" "-c" "$wincmd w | wincmd J | set modifiable write" ];
            merge-tool-edits-conflict-markers = true;
          };

          difftastic = {
            program = "${pkgs.difftastic}/bin/difft";
            diff-args = [ "--color=always" "$left" "$right" ];
          };
        };

        revsets = {
          log = "@ | ancestors(immutable_heads().., 2) | heads(immutable_heads())";
        };

        aliases =
          let
            mkExecAlias = program: args: [ "util" "exec" "--" program ] ++ args;
            mkHelpersAlias = subcommandName: mkExecAlias jj-helpers [ subcommandName ];
          in
          {
            "ui" = mkExecAlias "${pkgs.jj-fzf}/bin/jj-fzf" [ ];
            "worklog" = [ "log" "-r" "(trunk()..@):: | (trunk()..@)-" ];
            "reword" = mkHelpersAlias "reword";
            "change-id" = mkHelpersAlias "change-id";
            "commit-id" = mkHelpersAlias "commit-id";
            "description" = mkHelpersAlias "description";
            "bookmark-names" = mkHelpersAlias "bookmark-names";
            "run-job" = mkHelpersAlias "run-job";
            "pre-commit" = mkHelpersAlias "pre-commit";
            "flow" = mkHelpersAlias "flow";
          };

        revset-aliases = {
          # graph utilities
          "symdiff(x, y)" = "(x ~ y) | (y ~ x)"; # commits in either x or y, but not both
          "lr(x, y)" = "fork_point(x | y)..(x | y)"; # lr(x, y) is what 'git log' calls x...y
          "vee(x, y)" = "fork_point(x | y) | (fork_point(x | y)..(x | y))";

          # work utilities
          "named()" = "bookmarks() | remote_bookmarks() | tags() | trunk()";
          "merged()" = "ancestors(named())";
          "unmerged()" = "~merged()";

          # commit info
          "user(x)" = "author(x) | committer(x)";
          "mine()" =
            let
              names = [
                "Nicole Wren"
                "Nicole Prindle"
              ];
              emails = [
                "nicole@wren.systems"
                "nprindle18@gmail.com"
                "wrenn@squareup.com"
                "nprindle@squareup.com"
              ];
              toAuthor = x: "author(exact:${builtins.toJSON x})";
            in
            builtins.concatStringsSep " | " (builtins.map toAuthor (emails ++ names));
        };

        templates = {
          draft_commit_description = ''
            concat(
              description,
              surround(
                "\nJJ: Files:\n", "",
                indent("JJ:     ", diff.summary()),
              ),
              "\n",
              "JJ: ignore-rest\n",
              diff.git(),
            )
          '';
        };

        template-aliases = { };

        git = {
          push-bookmark-prefix = lib.mkDefault "kanwren/push-";
          private-commits = lib.mkDefault ''description(regex:"^[xX]+:") | description(glob-i:"^wip:")'';
        };
      };
    };
  };
}
