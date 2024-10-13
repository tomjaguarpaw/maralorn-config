{ pkgs, lib, ... }:
{

  config.systemd.user.services.status-notify = {
    Unit.Description = "status-notify";
    Service = {
      ExecStart = lib.getExe' pkgs.status-script "status-notify";
      Restart = "always";
      RestartSec = "10s";
    };
    Unit.StartLimitIntervalSec = "60s";
    Install.WantedBy = [ "default.target" ];
  };
}
