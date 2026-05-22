{
  flake.modules.homeManager.ghostty =
    { lib, ... }:
    {
      programs.ghostty = {
        enable = true;
        systemd.enable = true;
        clearDefaultKeybinds = true; # prefer to use the window manager to splits/tabs/etc.
        settings = {
          font-size = 11.0;
          font-family = "FiraCode Nerd Font Mono";
          shell-integration-features = lib.concatStringsSep "," [
            "cursor"
            "sudo"
            "title"
            "ssh-env"
            "ssh-terminfo"
            "path"
          ];
          keybind = [
            "ctrl+shift+p=toggle_command_palette"
            "ctrl+shift+n=new_window"

            "ctrl+==increase_font_size:1"
            "ctrl++=increase_font_size:1"
            "ctrl+-=decrease_font_size:1"
            "ctrl+0=reset_font_size"

            "ctrl+shift+f=start_search"
            "ctrl+shift+c=copy_to_clipboard:mixed"
            "ctrl+shift+v=paste_from_clipboard"
            "ctrl+shift+j=write_screen_file:paste,plain"

            "shift+page_up=scroll_page_up"
            "shift+page_down=scroll_page_down"
            "shift+home=scroll_to_top"
            "shift+end=scroll_to_bottom"
            "ctrl+shift+page_up=jump_to_prompt:-1"
            "ctrl+shift+page_down=jump_to_prompt:1"
          ];
        };
      };
    };
}
