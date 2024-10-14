{ pkgs, lib, ... }:
{

  config.systemd.user.services.status-notify = {
    Unit.Description = "status-notify";
    Service.ExecStart = lib.getExe' pkgs.status-script "status-notify";
    Install.WantedBy = [ "default.target" ];
  };
}
