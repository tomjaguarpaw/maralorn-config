{ config, lib, pkgs , ... }:
with lib;
let
  inherit (config.m-0.private) me;
in {
options.m-0.update_tasks.enable = mkEnableOption "Update Tasks";
config = mkIf config.m-0.update_tasks.enable {
  systemd.user = {
    services.update_tasks = {
      Unit = {
        Description = "Update Tasks";
      };
      Service = {
        Type = "oneshot";
        Environment="PATH=${pkgs.taskwarrior}/bin:${pkgs.eventd}/bin";
        ExecStart= let
          update = pkgs.writeShellScriptBin "update" ''
            ${config.home.homeDirectory}/.cargo/bin/update_tasks
            ${pkgs.taskwarrior}/bin/task +PENDING due.before:now+1month prio: mod prio:L
            ${pkgs.taskwarrior}/bin/task +PENDING due.before:now+1week prio:L mod prio:M
            ${pkgs.taskwarrior}/bin/task +PENDING due.before:now+1day prio:M mod prio:H
            true
          ''; in "${update}/bin/update";
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
