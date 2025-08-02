{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.services.gitit;

  gititDefaults = {
    address = "127.0.0.1";
    port = 5001;
    use-cache = true;
    cache-dir = "/var/cache/gitit";
    log-file = "/var/log/gitit/gitit.log";
    repository-type = "Git";
    default-page-type = "Markdown";
    max-upload-size = "5000K";
    max-page-size = "5000K";
    mail-command = "";
    mime-types-file = "${pkgs.mime-types}/etc/mime.types";
  };

  gititOverrides = {
    repository-path = "${cfg.repoPath}";
    static-dir = "${cfg.staticPath}";
    templates-dir = "${cfg.templatesPath}";
  };

  renderGititConfig = attrs: let
    renderValue = x:
      if builtins.isNull x
      then ""
      else if builtins.isString x
      then
        if x == ""
        then ""
        else if builtins.match "^([[:space:]]|.*\n).*$" x == null
        then x
        else lib.concatMapStrings (s: "\n  > " + s) (lib.splitString "\n" (lib.trim x))
      else if builtins.isAttrs x
      then builtins.throw "renderGititConfig: unexpected nested attrs in config value"
      else if builtins.isBool x
      then
        if x
        then "yes"
        else "no"
      else builtins.toString x;
    renderPair = name: value: name + ":\t" + renderValue value + "\n";
    renderPairs = lib.concatMapAttrsStringSep "" renderPair;
    renderSection = name: attrs: "\n\n[" + name + "]\n" + renderPairs attrs;
    sections = builtins.partition (kv: builtins.isAttrs kv.value) (lib.attrsToList attrs);
    defaultSection = lib.listToAttrs sections.wrong;
    nestedSections = lib.listToAttrs sections.right;
  in
    renderPairs defaultSection + lib.concatMapAttrsStringSep "" renderSection nestedSections;

  configFiles =
    [
      (pkgs.writeText "gitit-default.conf" (renderGititConfig gititDefaults))
      (pkgs.writeText "gitit-custom.conf" (renderGititConfig cfg.config))
      (pkgs.writeText "gitit-overrides.conf" (renderGititConfig gititOverrides))
    ]
    ++ cfg.extraConfigFiles;
in {
  options.services.gitit = {
    enable = lib.mkEnableOption "Enable gitit wiki";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.gitit;
      description = ''
        The gitit package to use
      '';
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "gitit";
      description = ''
        The user under which gitit will run
      '';
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "gitit";
      description = ''
        The group under which gitit will run
      '';
    };

    baseDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/gitit";
      description = ''
        The base directory in which gitit stores its data
      '';
    };

    repoPath = lib.mkOption {
      type = lib.types.path;
      default = "${config.services.gitit.baseDir}/wikidata";
      description = ''
        The path to the gitit wiki repository; this is where gitit will store its wiki pages.
      '';
    };

    staticPath = lib.mkOption {
      type = lib.types.path;
      default = "${config.services.gitit.repoPath}/static";
      description = ''
        The path where gitit keeps its static files. By default, they are managed in the repo.
      '';
    };

    templatesPath = lib.mkOption {
      type = lib.types.path;
      default = "${config.services.gitit.repoPath}/templates";
      description = ''
        The path where gitit keeps its templates. By default, they are managed in the repo.
      '';
    };

    config = lib.mkOption {
      type = let
        atom = lib.types.oneOf [lib.types.str lib.types.bool lib.types.int];
      in
        lib.types.attrsOf (lib.types.either atom (lib.types.attrsOf atom));
      default = {};
      description = ''
        Configuration for gitit, in Haskell syntax
      '';
    };

    extraConfigFiles = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = ''
        Additional configuration files to include in the gitit configuration.
        These files will be included in the order they are specified, later
        files taking precedence.
      '';
    };

    autopush = {
      enable = lib.mkEnableOption "Enable automatic git push after changes";

      remote = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = ''
          The git remote to push changes to after modifications.
        '';
      };

      remotePath = lib.mkOption {
        type = lib.types.path;
        default = "";
        description = ''
          Path to file containing git repository to push changes to after modifications.
        '';
      };

      deployKeyPath = lib.mkOption {
        type = lib.types.path;
        description = ''
          The path to the SSH deploy key used for pushing changes to the git repository.
          This key should have write access to the gitit wiki repository.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.autopush.enable -> (cfg.autopush.remote != "" || cfg.autopush.remotePath != "");
        message = "one of services.gitit.autopush.remote or services.gitit.autopush.remotePath must be set when authPush is enabled";
      }
      {
        assertion = cfg.autopush.enable -> (cfg.autopush.remote == "" || cfg.autopush.remotePath == "");
        message = "cannot set both services.gitit.autopush.remote and services.gitit.autopush.remotePath";
      }
    ];

    systemd.services.gitit = {
      description = "Gitit wiki";
      script = ''
        ${lib.getExe' cfg.package "gitit"} ${lib.strings.escapeShellArgs (builtins.map (x: "--config-file=${x}") configFiles)}
      '';
      postStart = let
        gitExe = lib.escapeShellArg (lib.getExe' pkgs.git "git");
        installExe = lib.escapeShellArg (lib.getExe' pkgs.coreutils "install");
        repoPath = lib.escapeShellArg cfg.repoPath;
        remotePath =
          if cfg.autopush.remotePath != ""
          then ''"$(cat ${lib.escapeShellArg cfg.autopush.remotePath})"''
          else lib.escapeShellArg cfg.autopush.remote;
        postCommit = pkgs.writers.writeBashBin "gitit-post-commit-push" ''
          set -euo pipefail
          export GIT_SSH_COMMAND="${lib.getExe' pkgs.openssh "ssh"} -o 'StrictHostKeyChecking no' -i ${cfg.autopush.deployKeyPath}"
          remote=${remotePath}
          ${gitExe} pull "$remote" @ --rebase || true
          ${gitExe} -C ${repoPath} push "$remote" @ || true
        '';
        postCommitScript = lib.escapeShellArg (lib.getExe postCommit);
      in
        if cfg.autopush.enable
        then ''
          ${installExe} -Dm755 ${postCommitScript} ${repoPath}'/.git/hooks/post-commit'
        ''
        else ''
          rm -rf ${repoPath}'/.git/hooks/post-commit'
        '';
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        User = "gitit";
        Group = "gitit";
        CacheDirectory = "gitit";
        StateDirectory = "gitit";
        LogsDirectory = "gitit";
        WorkingDirectory = cfg.baseDir;
        SyslogIdentifier = "gitit";
      };
      path = [pkgs.git];
      wantedBy = ["default.target"];
    };

    users = {
      users = lib.mkIf (cfg.user == "gitit") {
        gitit = {
          name = "gitit";
          group = "gitit";
          isSystemUser = true;
        };
      };
      groups = lib.mkIf (cfg.group == "gitit") {
        gitit = {};
      };
    };
  };
}
