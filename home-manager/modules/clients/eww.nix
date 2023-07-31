{
  pkgs,
  lib,
  config,
  ...
}:
{

  systemd.user.services = {
    status-script = {
      Unit = {
        Description = "status-script";
      };
      Service = {
        Environment = "PATH=${
            lib.makeBinPath [
              pkgs.coreutils
              pkgs.hyprland
              pkgs.jq
              pkgs.socat
              pkgs.bash
              pkgs.status-script
              pkgs.curl
              pkgs.openssh
              pkgs.gitMinimal
              pkgs.nix
              pkgs.taskwarrior
              config.programs.rbw.package
            ]
          }";
        ExecStart = lib.getExe pkgs.status-script;
        Restart = "always";
        RestartSec = "10s";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
    eww = {
      Unit = {
        X-Restart-Triggers = [ "${config.programs.eww.configDir}" ];
        Description = "EWW";
        Wants = [ "status-script.service" ];
        After = [ "status-script.service" ];
      };

      Service = {
        Environment = "PATH=${
            lib.makeBinPath [
              pkgs.coreutils
              pkgs.hyprland
              pkgs.jq
              pkgs.socat
              pkgs.bash
              pkgs.curl
              pkgs.openssh
              pkgs.gitMinimal
              pkgs.nix
              pkgs.jaq
              config.programs.rbw.package
            ]
          }";
        ExecStart = "${
            lib.getExe config.programs.eww.package
          } daemon --no-daemonize --restart";
        ExecStartPost = "${lib.getExe config.programs.eww.package} open bar";
        Restart = "always";
        RestartSec = "10s";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
  programs.eww = {
    enable = true;
    package = pkgs.callPackage ./_eww-package.nix { withWayland = true; };
    configDir = ./eww-config;
  };
}
