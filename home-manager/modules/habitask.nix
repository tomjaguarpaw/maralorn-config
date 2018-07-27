{ config, lib, pkgs , ... }:
with lib;
let
  inherit (config.m-0.private) habitica;
in {
options.m-0.habitask.enable = mkEnableOption "Run Habitask";
config = mkIf config.m-0.habitask.enable {
  systemd.user = {
    services.habitask = {
      Unit = {
        Description = "Run Habitask";
      };
      Service = {
        Type = "oneshot";
        Environment="PATH=${pkgs.taskwarrior}/bin HABITASK_USER=${habitica.user} HABITASK_KEY=${habitica.key}";
        ExecStart="${pkgs.habitask}/bin/habitask";
      };
    };
    timers.update_tasks = {
      Timer = {
        OnCalendar = "19:56:00";
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
};

}
