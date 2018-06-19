{ config, lib, pkgs , ... }:
with lib;
let

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
        ExecStart="${pkgs.rust_scripts}/bin/update_tasks";
      };
    };
    timers.update_tasks = {
      Timer = {
        OnCalendar = "hourly";
      };
    };
  };
};

}
