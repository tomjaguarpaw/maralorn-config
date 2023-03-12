{
  pkgs,
  lib,
  ...
}: {
  systemd.user.services.conky = {
    Unit.Description = "Run conky";
    Service = {
      ExecStart = "${lib.getExe pkgs.conky} -c ${./conky.conf}";
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = ["default.target"];
  };
}
