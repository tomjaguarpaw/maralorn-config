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
      Before = [ "kanshi.service" ];
      PartOf = [ "graphical-session.target" ];
      Requires = [ "graphical-session.target" ];
    };
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      Environment = "PATH=${
        lib.makeBinPath [
          pkgs.coreutils
          pkgs.hyprland
          pkgs.jq
          pkgs.socat
          pkgs.bash
          pkgs.curl
          pkgs.gitMinimal
          pkgs.nix
          pkgs.jaq
          pkgs.t
          config.programs.rbw.package
        ]
      }";
      ExecStart = "${lib.getExe config.programs.eww.package} daemon --no-daemonize --restart";
      ExecStartPost = [ "${lib.getExe' pkgs.kanshi "kanshictl"} reload" ];
      Restart = "always";
      RestartSec = "10s";
    };
  };

  programs.eww = {
    enable = true;
    package = pkgs.eww-wayland;
    configDir = ./eww-config;
  };
}
