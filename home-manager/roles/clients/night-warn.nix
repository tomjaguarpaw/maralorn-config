{pkgs, lib, ...}:
{
  systemd.user = {
    timers.night-warn = {
      Timer.OnCalendar = "23:00";
      Install.WantedBy = ["timers.target"];
    };
    services.night-warn.Service.ExecStart = ''${lib.getExe pkgs.set-timer} Wecker "tomorrow 8:45"'';
  };
}
