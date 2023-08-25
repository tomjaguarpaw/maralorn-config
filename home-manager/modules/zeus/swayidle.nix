{ pkgs, lib, ... }:
{
  systemd.user.services.swayidle = {
    Unit.Description = "swayidle";
    Service = {
      Environment = "PATH=${
          lib.makeBinPath [
            pkgs.coreutils
            pkgs.hyprland
            pkgs.systemd
          ]
        }";
      ExecStart = pkgs.writeShellScript "my-swayidle" ''
        ${
          lib.getExe pkgs.swayidle
        }  -w timeout 1 'echo "{\"at\": $(date +%s), \"idle\": true}" > ~/.idle_state' resume 'echo "{\"at\": $(date +%s), \"idle\": false}" > ~/.idle_state' timeout 300 'hyprctl dispatch dpms off' resume 'hyprctl dispatch dpms on' timeout 600 'systemctl suspend'
      '';
      Restart = "always";
      RestartSec = "10s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
