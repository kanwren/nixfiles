{
  programs.starship = {
    enable = true;
    settings = {
      git_status =
        let
          style = s: v: "[${v}](${s})";
          withCount = v: "${v}\${count}";
        in
        {
          format = "([\\[$ahead_behind$conflicted$stashed$deleted$renamed$modified$staged$untracked\\]]($style) )";
          # Note: all_status doesn't work anymore, inlined above
          # all_status = "$conflicted$stashed$deleted$renamed$modified$staged$untracked";
          ahead = style "bold bright-green" (withCount "⇡");
          behind = style "bold bright-green" (withCount "⇣");
          diverged = style "bold bright-green" "⇡$ahead_count⇣$behind_count";
          stashed = style "bold bright-blue" (withCount "\\$");
          staged = style "bold yellow" (withCount "+");
          modified = style "bold yellow" (withCount "!");
          deleted = style "bold yellow" (withCount "✘");
          renamed = style "bold yellow" (withCount "»");
          untracked = style "bold bright-red" (withCount "?");
          conflicted = style "bold bright-red" (withCount "≠");
        };
      git_commit = {
        tag_disabled = false;
        tag_symbol = "";
      };
      kubernetes = {
        disabled = false;
      };
    };
  };
}
