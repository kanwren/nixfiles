{ self, system, ... }:

{
  programs.kitty = {
    enable = true;

    settings = {
      # Font
      font_family = "FiraCode Nerd Font Mono";
      italic_font = "FiraCode Nerd Font Mono Italic";
      bold_font = "FiraCode Nerd Font Mono Bold";
      bold_italic_font = "FiraCode Nerd Font Mono Bold Italic";

      font_size = "10.0";

      # display
      # cursor, cursor_text_color
      scrollback_lines = 10000;

      # input
      mouse_hide_wait = "3.0";
      input_delay = 0;

      # bells
      enable_audio_bell = false;
      visual_bell_duration = "0.0";
      window_alert_on_bell = false;
      bell_on_tab = false;
      command_on_bell = "none";

      # layout
      remember_window_size = true;
      draw_minimal_borders = true;
      window_padding_width = 5; # also window_border_width, window_margin_width
      placement_strategy = "center";
      active_border_color = "none"; # also inactive_border_color

      # advanced
      shell = ".";
      editor = ".";
      term = "xterm-kitty";
      kitty_mod = "ctrl+shift";
      clear_all_shortcuts = true;
    };

    keybindings = {
      "kitty_mod+c" = "copy_to_clipboard";
      "kitty_mod+v" = "paste_from_clipboard";
      "shift+insert" = "paste_from_selection";
      "kitty_mod+equal" = "change_font_size all +1.0";
      "kitty_mod+minus" = "change_font_size all -1.0";
      "kitty_mod+backspace" = "change_font_size all 0";
      "kitty_mod+e" = "kitten hints";
      "kitty_mod+p>f" = "kitten hints --type path --program -";
      "kitty_mod+p>shift+f" = "kitten hints --type path";
      "kitty_mod+p>l" = "kitten hints --type line --program -";
      "kitty_mod+p>h" = "kitten hints --type hash --program -";
      "kitty_mod+u" = "kitten unicode_input";
      "kitty_mod+escape" = "kitty_shell window";
    };

    extraConfig = ''
      include ${self.packages.${system}.catppuccin-kitty}/mocha.conf
    '';
  };
}

