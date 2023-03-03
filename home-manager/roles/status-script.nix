{
  pkgs,
  lib,
  ...
}: {
  systemd.user.services.status-script = {
    Unit.Description = "Status Script";
    Service.ExecStart = lib.getExe pkgs.status-script;
    Install.WantedBy = ["graphical-session.target"];
  };
}
