{ pkgs, config, ... }:
let

  configPath = "${config.home.homeDirectory}/git/config";
  configGit = "${pkgs.git}/bin/git -C ${configPath}";
  script = pkgs.writeShellScript "hourly-maintenance" ''
    set -e
    ${configGit} fetch
    if [[ "$(${configGit} rev-parse master)" == "$(${configGit} rev-parse origin/master)" ]]; then
      echo "Git repo up-to-date, not doing anything."
      exit 0;
    else
      ${pkgs.kitty}/bin/kitty --hold ${config.home.profileDirectory}/bin/maintenance
    fi
  '';
in
{
  systemd.user = {
    services.maintenance = {
      Unit.Description = "Routine maintenance";
      Service = {
        Type = "oneshot";
        ExecStart = toString script;
      };
    };
    timers.maintenance = {
      Unit.Description = "Hourly maintenance";
      Timer.OnCalendar = "hourly";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
