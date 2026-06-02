{
  flake.modules.homeManager.jujutsu = {
    programs.fish.shellAbbrs =
      let
        commandAbbr = cmd: {
          position = "command";
          expansion = cmd;
        };
      in
      {
        "-T" = {
          command = "jj";
          position = "anywhere";
          setCursor = "%";
          expansion = "--no-graph --template '%'";
        };

        "j/" = commandAbbr "jj split";
        "j/p" = commandAbbr "jj split --parallel";
        "ja" = commandAbbr "jj absorb";
        "jb" = commandAbbr "jj bookmark";
        "jbc" = commandAbbr "jj bookmark create";
        "jbd" = commandAbbr "jj bookmark delete";
        "jbf" = commandAbbr "jj bookmark forget";
        "jbl" = commandAbbr "jj bookmark list";
        "jbm" = commandAbbr "jj bookmark move";
        "jbn" = commandAbbr "jj bookmark-names";
        "jbr" = commandAbbr "jj bookmark rename";
        "jbs" = commandAbbr "jj bookmark set";
        "jbt" = commandAbbr "jj bookmark track";
        "jbu" = commandAbbr "jj bookmark untrack";
        "jc" = commandAbbr "jj commit";
        "jcf" = commandAbbr "jj config";
        "jcfl" = commandAbbr "jj config list";
        "jci" = commandAbbr "jj commit --interactive";
        "jcm" = commandAbbr "jj commit --message";
        "jd" = commandAbbr "jj describe";
        "jde" = commandAbbr "jj diffedit";
        "jdf" = commandAbbr "jj diff";
        "jdup" = commandAbbr "jj duplicate";
        "je" = commandAbbr "jj edit";
        "jf" = commandAbbr "jj file";
        "jfa" = commandAbbr "jj file annotate";
        "jfc" = commandAbbr "jj file chmod";
        "jfl" = commandAbbr "jj file list";
        "jfs" = commandAbbr "jj file show";
        "jft" = commandAbbr "jj file track";
        "jfu" = commandAbbr "jj file untrack";
        "jg" = commandAbbr "jj git";
        "jgf" = commandAbbr "jj git fetch";
        "jgi" = commandAbbr "jj git init";
        "jgp" = commandAbbr "jj git push";
        "jid" = commandAbbr "jj id";
        "jk" = commandAbbr "jj abandon";
        "jl" = commandAbbr "jj log";
        "jlr" = commandAbbr "jj log --revisions";
        "jm" = commandAbbr "jj metaedit";
        "jmc" = commandAbbr "jj metaedit --update-change-id";
        "jmm" = commandAbbr "jj mm";
        "jmmca" = commandAbbr "jj mm changes add";
        "jmmcc" = commandAbbr "jj mm changes clean-empty";
        "jmmcm" = commandAbbr "jj mm changes move";
        "jmmcr" = commandAbbr "jj mm changes remove";
        "jmmp" = commandAbbr "jj mm push";
        "jmmr" = commandAbbr "jj mm rebase";
        "jmmrt" = commandAbbr "jj mm rebase 'trunk()'";
        "jmmt" = commandAbbr "jj mm tip";
        "jn" = commandAbbr "jj new";
        "jna" = commandAbbr "jj new --no-edit --insert-after";
        "jnb" = commandAbbr "jj new --no-edit --insert-before";
        "jne" = commandAbbr "jj new --no-edit";
        "jnt" = commandAbbr "jj new 'trunk()'";
        "joa" = commandAbbr "jj operation abandon";
        "jod" = commandAbbr "jj operation diff";
        "jol" = commandAbbr "jj operation log";
        "jop" = commandAbbr "jj operation";
        "jor" = commandAbbr "jj operation restore";
        "jos" = commandAbbr "jj operation show";
        "jox" = commandAbbr "jj operation revert";
        "jr" = commandAbbr "jj rebase";
        "jra" = commandAbbr "jj rebase --insert-after";
        "jrb" = commandAbbr "jj rebase --insert-before";
        "jrt" = commandAbbr "jj rebase --destination 'trunk()'";
        "js" = commandAbbr "jj show";
        "jsp" = commandAbbr "jj simplify-parents";
        "jspr" = commandAbbr "jj simplify-parents --revisions";
        "jsq" = commandAbbr "jj squash";
        "jt" = commandAbbr "jj tag";
        "jtl" = commandAbbr "jj tag list";
        "ju" = commandAbbr "jj undo";
        "jv" = commandAbbr "jj parallelize";
        "jw" = commandAbbr "jj workspace";
        "jwa" = commandAbbr "jj workspace add";
        "jwf" = commandAbbr "jj workspace forget";
        "jwl" = commandAbbr "jj workspace list";
        "jwr" = commandAbbr "jj workspace rename";
        "jwu" = commandAbbr "jj workspace update-stale";
        "jx" = commandAbbr "jj revert";
        "jz" = commandAbbr "jj restore";
        "jzd" = commandAbbr "jj restore --restore-descendants";
        "jzi" = commandAbbr "jj restore --interactive";
      };
  };
}
