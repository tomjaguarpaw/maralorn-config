{ config, lib, pkgs , ... }:
with lib;
let
  inherit (config.m-0.private) me gitlab github otrs;
in {
options.m-0.bugwarrior.enable = mkEnableOption "Sync tasks from issuetrackers";
config = mkIf config.m-0.bugwarrior.enable {
  home.file.".config/bugwarrior/bugwarriorrc".text = ''
    [general]
    targets=cda_gitlab,github
    static_fields = priority, project

    [cda_gitlab]
    service=gitlab
    gitlab.password=@oracle:eval:pass de/darmstadt/ccc/ldap
    gitlab.login=${me.user}
    gitlab.host=${gitlab.host}
    gitlab.token=${gitlab.token}
    gitlab.only_if_assigned=${me.user}
    gitlab.also_unassigned=True
    gitlab.default_priority= L
    gitlab.include_repos=cda/tasks, cda/chaos-darmstadt.de, cda/doku
    gitlab.include_regex=(vorstand|jali|${me.user})/.*,
    gitlab.description_template = {{gitlabnamespace}}/{{gitlabrepo}} {{gitlabtype[:1]}}#{{gitlabnumber}}: {{gitlabtitle}}
    gitlab.project_template =

    [github]
    service=github
    github.login = ${me.user}
    github.username = ${me.user}
    github.token = ${github.token}
    github.description_template = {{githubrepo}} {{githubtype[:1]}}#{{githubnumber}}: {{githubtitle}}
    github.include_user_issues = False
    github.include_user_repos = False
    github.query = is:open involves:maralorn archived:false -repo:maxtaco/coffee-script -repo:QMatrixClient/libqmatrixclient -repo:QMatrixClient/Quaternion -repo:trollhoehle/pythonlights -repo:MirakelX/mirakel-android
    github.default_priority= L
    github.project_template =
  '';
  systemd.user = {
    services.bugwarrior = {
      Unit = {
        Description = "Run bugwarrior";
      };
      Service = {
        Type = "oneshot";
        Environment=''PATH=${pkgs.taskwarrior}/bin:${pkgs.eventd}/bin:${pkgs.gnugrep}/bin OTRS_USER=${me.user} OTRS_PASSWORD=${otrs.password} OTRS_QUEUES="${otrs.queues}" OTRS_OWNERS="${otrs.owners}" OTRS_HOST=${otrs.host}'';
        ExecStart= let
          update = pkgs.writeShellScriptBin "update" ''
            ${pkgs.bugwarrior}/bin/bugwarrior-pull
            ${pkgs.rust_scripts}/bin/sync_otrs
            true
          ''; in "${update}/bin/update";
      };
    };
    timers.bugwarrior = {
      Timer = {
        OnCalendar = "hourly";
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
};

}
