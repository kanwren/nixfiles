{ self }:

{ pkgs, ... }:

{
  home.packages = [
    pkgs.fishPlugins.foreign-env
    pkgs.fishPlugins.fzf-fish
    self.packages.${pkgs.system}.wd-fish
  ];

  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      # set prompt to '; '
      function fish_prompt
        if [ $status = 0 ]
          set_color --bold green
        else
          set_color --bold red
        end
        printf '; '
        set_color normal
      end

      function fish_mode_prompt; end

      # don't greet
      function fish_greeting; end

      # use vi bindings
      fish_vi_key_bindings

      # restore last directory when new shell is opened
      set --query fish_most_recent_dir
      and test -d "$fish_most_recent_dir"
      and cd "$fish_most_recent_dir"

      function save_dir --on-variable PWD
        set -U fish_most_recent_dir $PWD
      end
    '';
  };
}
