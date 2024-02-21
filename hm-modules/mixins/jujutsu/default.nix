{
  programs.jujutsu = {
    enable = true;

    settings = {
      user = {
        name = "Nicole Wren";
        email = "nicole@wren.systems";
      };

      ui = {
        pager = "less -RF";
        paginate = "auto";
        diff-editor = ":builtin";
        default-command = "worklog";
      };

      revsets = {
        log = "@ | ancestors(immutable_heads().., 2) | heads(immutable_heads())";
      };

      aliases = {
        "worklog" = [ "log" "-r" "(trunk()..@):: | (trunk()..@)-" ];
      };

      revset-aliases = {
        # graph utilities
        "xor(x, y)" = "(x ~ y) | (y ~ x)"; # commits in either x or y, but not both
        "bases(x, y)" = "heads(::x & ::y)"; # latest base commit of both x and y
        "fork(x, y)" = "heads(::x & ::y)::(x | y)"; # the bases of x and y, along with the paths to x and y; bases(x, y) | lr(x, y)
        "lr(x, y)" = "(::x ~ ::y) | (::y ~ ::x)"; # lr(x, y) is what 'git log' calls x...y

        # commit info
        "user(x)" = "author(x) | committer(x)";
        "mine" =
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

      template-aliases = { };

      git = {
        push-branch-prefix = "kanwren/push-";
      };
    };
  };
}
