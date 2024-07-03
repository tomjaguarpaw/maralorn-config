{
  pkgs,
  lib,
  config,
  ...
}:
{

  systemd.user.services.eww = {
    Unit = {
      X-Restart-Triggers = [ "${config.programs.eww.configDir}" ];
      Description = "EWW";
      Wants = [ "status-script.service" ];
      After = [
        "status-script.service"
        "graphical-session.target"
      ];
      PartOf = [ "graphical-session.target" ];
      Requires = [ "graphical-session.target" ];
      StartLimitIntervalSec = "60s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      Environment = "PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.jq
          pkgs.socat
          pkgs.bash
          pkgs.curl
          pkgs.gitMinimal
          pkgs.nix
          pkgs.jaq
          pkgs.t
          pkgs.hyprland
          config.programs.rbw.package
        ]
      }";
      ExecStart = "${lib.getExe config.programs.eww.package} daemon --no-daemonize --restart";
      ExecStartPost = "${lib.getExe config.programs.eww.package} open topbar";
      Restart = "always";
      RestartSec = "10s";
    };
  };

  programs.eww = {
    enable = true;
    configDir = ./eww-config;
  };
}
