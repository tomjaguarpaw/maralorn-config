{ pkgs, lib, ... }:
{
  systemd.user = {
    services.fetch-banking = {
      Unit.Description = "Fetch banking";
      Service = {
        Type = "oneshot";
        Environment = "PATH=${
            lib.makeBinPath [
              pkgs.coreutils
              pkgs.git
            ]
          }";
        WorkingDirectory = "%h/git/buchhaltung";
        ExecStart = "${lib.getExe pkgs.nix} develop -c unsupervised-update";
      };
    };
    timers.fetch-banking = {
      Unit.Description = "Fetch banking";
      Timer.OnCalendar = "22:00";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
