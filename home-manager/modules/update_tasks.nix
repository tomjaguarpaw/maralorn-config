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
        Description = "Update taskwarrior tasks";
      };
      Service = {
        Type = "oneshot";
        Environment="PATH=${pkgs.taskwarrior}/bin:${pkgs.eventd}/bin";
        ExecStart= "${config.home.homeDirectory}/.cargo/bin/update_tasks";
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
