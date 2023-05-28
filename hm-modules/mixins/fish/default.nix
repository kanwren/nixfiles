{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      # set prompt to '; '
      function fish_prompt; echo -n '; '; end
      function fish_mode_prompt; end

      # don't greet
      function fish_greeting; end

      # use vi bindings
      fish_vi_key_bindings

      # restore last directory when new shell is opened
      set -q fish_most_recent_dir; and [ -d "$fish_most_recent_dir" ]; and cd "$fish_most_recent_dir"
      function save_dir --on-variable PWD
        set -U fish_most_recent_dir $PWD
      end
    '';
  };
}
