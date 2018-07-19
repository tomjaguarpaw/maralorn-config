{ config, lib, pkgs , ... }:
with lib;
let
  inherit (config.m-0.private) me gitlab;
in {
options.m-0.update_tasks.enable = mkEnableOption "Update Tasks";
config = mkIf config.m-0.update_tasks.enable {
  home.file.".config/bugwarrior/bugwarriorrc".text = ''
    [general]
    targets=cda_gitlab

    [cda_gitlab]
    service=gitlab
    gitlab.password=@oracle:eval:pass de/darmstadt/ccc/ldap
    gitlab.login=${me.user}
    gitlab.host=${gitlab.host}
    gitlab.token=${gitlab.token}
    gitlab.only_if_assigned=${me.user}
    gitlab.only_if_author=${me.user}
  '';
  home.packages = [ pkgs.python2Packages.bugwarrior ];
  systemd.user = {
    services.update_tasks = {
      Unit = {
        Description = "Update Tasks";
      };
      Service = {
        Type = "oneshot";
        Environment="PATH=${pkgs.taskwarrior}/bin:${pkgs.eventd}/bin";
        ExecStart="${pkgs.rust_scripts}/bin/update_tasks";
      };
    };
    timers.update_tasks = {
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
