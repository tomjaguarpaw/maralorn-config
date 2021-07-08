{ pkgs, config, ... }:
{
  systemd.user = {
    services.maintenance = {
      Unit.Description = "Routine maintenance";
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.kitty}/bin/kitty --hold ${config.home.profileDirectory}/bin/maintenance --only-on-update";
      };
    };
    timers.maintenance = {
      Unit.Description = "Hourly maintenance";
      Timer.OnCalendar = "hourly";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
