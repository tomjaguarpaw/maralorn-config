{ pkgs, lib, ... }:
{

  config.systemd.user.services.vikunja-watch = {
    Service = {
      ExecStart = lib.getExe' pkgs.vikunja-tools "vikunja-watch";
      Restart = "always";
      RestartSec = "10s";
    };
    Unit.StartLimitIntervalSec = "60s";
    Install.WantedBy = [ "default.target" ];
  };
}
