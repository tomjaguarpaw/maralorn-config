{pkgs, lib, ...}:
let
  idle-timeout = 150;
  write-idle = pkgs.writeShellScript "write-idle" ''
    echo "{\"contents\": $(${lib.getBin pkgs.coreutils}/bin/date +%s -d "-${toString idle-timeout} seconds"), \"tag\": \"Idle\"}" > ~/.idle_state
  '';
  write-active = pkgs.writeShellScript "write-active" ''
    echo "{\"tag\":\"Active\"}" > ~/.idle_state
  '';
in
{
  services.swayidle = {
    enable = true;
    systemdTarget = "graphical-session.target";
    timeouts = [
      {
        timeout = idle-timeout;
        command = write-idle.outPath;
        resumeCommand = write-active.outPath;
      }
      {
        timeout = 300;
        command = "${lib.getBin pkgs.hyprland}/bin/hyprctl dispatch dpms off";
        resumeCommand = "${lib.getBin pkgs.hyprland}/bin/hyprctl dispatch dpms on";
      }
      {
        timeout = 599;
        command = "${lib.getBin pkgs.hyprland}/bin/hyprctl dispatch dpms on";
      }
      {
        timeout = 600;
        command = "${lib.getBin pkgs.systemd}/bin/systemctl suspend";
      }
    ];
  };
}
