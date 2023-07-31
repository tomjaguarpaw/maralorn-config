{
  pkgs,
  config,
  lib,
  ...
}:
let
  configPath = "${config.home.homeDirectory}/git/config";
  configGit = "${pkgs.gitMinimal}/bin/git -C ${configPath}";
in
{
  systemd.user = {
    services.refresh-config = {
      Unit.Description = "Fetch config repo";
      Service = {
        Environment = "PATH=${
            lib.makeBinPath [
              pkgs.coreutils
              pkgs.bash
              pkgs.openssh
              pkgs.gitMinimal
            ]
          }";
        Type = "oneshot";
        ExecStart = "${configGit} fetch origin main";
      };
    };
    timers.refresh-config = {
      Unit.Description = "Fetch config updates";
      Timer.OnCalendar = "*:0/5:0";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
