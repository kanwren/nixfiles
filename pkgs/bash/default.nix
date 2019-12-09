{ config, pkgs, ... }:

rec {
  environment = {
    systemPackages = with pkgs; [ bashInteractive ];

    interactiveShellInit = builtins.readFile ./bashrc;
  };

  programs.bash = {
    # This gets appended to the interactiveShellInit
    interactiveShellInit = "";

    enableCompletion = true;

    shellAliases = with pkgs; {
      ls = "${coreutils}/bin/ls --color=auto";
      grep = "${gnugrep}/bin/grep --color=auto";
      fgrep = "${gnugrep}/bin/fgrep -F --color=auto";
      egrep = "${gnugrep}/bin/egrep -E --color=auto";
      diff = "${colordiff}/bin/colordiff";

      ".." = "cd ..";
      ".1" = "cd ..";
      ".2" = "cd ../..";
      ".3" = "cd ../../..";
      ".4" = "cd ../../../..";
      ".5" = "cd ../../../../..";
      ".6" = "cd ../../../../../..";
      ".7" = "cd ../../../../../../..";
      ".8" = "cd ../../../../../../../..";
      ".9" = "cd ../../../../../../../../..";

      cleantex = "rm *.aux *.log";
      copylast = "fc -ln -1 | ${gawk}/bin/awk '{\$1=\$1}1' | ${xclip}/bin/xclip -sel clip";
      xc = "${xclip}/bin/xclip -sel clip";
      getpass = "${openssl}/bin/openssl rand -base64";

      nrn = "${nix}/bin/nix repl '<nixpkgs>' '<nixpkgs/nixos>'";
    };
  };

}

