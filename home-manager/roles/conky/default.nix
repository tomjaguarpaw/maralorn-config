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
    };
    Install.WantedBy = ["default.target"];
  };
}
