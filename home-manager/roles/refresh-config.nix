{
  pkgs,
  config,
  ...
}: let
  configPath = "${config.home.homeDirectory}/git/config";
  configGit = "${pkgs.git}/bin/git -C ${configPath}";
in {
  systemd.user = {
    services.update-config = {
      Unit.Description = "Fetch config repo";
      Service = {
        Type = "oneshot";
        ExecStart = "${configGit} fetch origin main";
      };
    };
    timers.update-config = {
      Unit.Description = "Fetch config updates";
      Timer.OnCalendar = "minutely";
      Install.WantedBy = ["timers.target"];
    };
  };
}
