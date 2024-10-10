{
  pkgs,
  lib,
  config,
  ...
}:
let
  open-bars = pkgs.writeShellScript "open-bars" ''
    open-bars () {
      if [[ "$(wlr-randr --json | jq 'map(select(.enabled)) | length')" == "2" ]]; then
        eww open-many topbar-0 topbar-1
      else
        eww open topbar-0
      fi
    }

    open-bars
    socat -u UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock - | rg monitoradded --line-buffered | while read; do
      open-bars
    done
  '';
in
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
          pkgs.hyprland
          pkgs.wlr-randr
          pkgs.ripgrep
          config.programs.eww.package
          config.programs.rbw.package
        ]
      }";
      ExecStart = pkgs.writeShellScript "run-eww-with-bars" ''
        ${open-bars} &
        ${lib.getExe config.programs.eww.package} daemon --no-daemonize --restart
      '';
      ExecStartPost = [ ];
      Restart = "always";
      RestartSec = "10s";
    };
  };

  programs.eww = {
    enable = true;
    configDir = ./eww-config;
  };
}
