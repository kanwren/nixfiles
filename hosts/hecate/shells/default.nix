{
  programs = {
    command-not-found.enable = false;
    fish = {
      enable = true;
      shellAbbrs = {
        xy = "xsel -ib";
        xp = "xsel -ob";
      };
    };
  };
}
