{ writeShellScriptBin
, firefox
, addmeta
}:

let
  script = writeShellScriptBin "firefox-app" ''
    set -euo pipefail

    profile_path="$HOME/.local/share/firefox-profiles/app"
    chrome_path="$profile_path/chrome"

    if [ ! -d "$profile_path" ]; then
      echo "Initializing Firefox app mode..."
      mkdir -p "$profile_path" "$chrome_path"

      echo "Enabling userChrome.css..."
      cat << EOF >> "$profile_path"/prefs.js
    user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
    EOF

      echo "Writing custom userChrome.css..."
      cat << EOF > "$chrome_path"/userChrome.css
    @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");
    #TabsToolbar { visibility: collapse !important; }
    #BookmarksToolbar { visibility: collapse !important; }
    #nav-bar { visibility: collapse !important; }
    #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
      visibility: collapse !important;
    }
    EOF

      echo "Done."
    fi

    ${firefox}/bin/firefox -profile "$profile_path" "$@"
  '';
in
addmeta script {
  description = "Hack to get firefox to open like chrome's --app";
}

