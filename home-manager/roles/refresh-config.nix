{ pkgs, config, ... }:
let
  configPath = "${config.home.homeDirectory}/git/config";
  configGit = "${pkgs.git}/bin/git -C ${configPath}";
in
{
  systemd.user = {
    services.refresh-config = {
      Unit.Description = "Fetch config repo";
      Service = {
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
