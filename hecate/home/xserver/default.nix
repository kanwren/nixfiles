{ ... }:

{
  xserver.xcompose = {
    enable = true;
    includeLocale = true;
    mappings = [
      { keys = [ "0" "space" ]; result = "​"; }
      { keys = [ "space" "0" ]; result = "​"; }
    ];
  };
}

