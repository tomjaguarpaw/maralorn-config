{
  pkgs,
  lib,
  config,
  ...
}:
let
  idle-timeout = 150;
  write-idle = pkgs.writeShellScript "write-idle" ''
    ${lib.getExe pkgs.jq} "{contents: $(${lib.getExe' pkgs.coreutils "date"} +%s -d "-${toString idle-timeout} seconds"), tag: \"Idle\"}" -n > ~/.idle_state
  '';
  write-active = pkgs.writeShellScript "write-active" ''
    ${lib.getExe pkgs.jq} "{tag:\"Active\"}" -n > ~/.idle_state
  '';
in
{
  services.swayidle = {
    enable = true;
    systemdTarget = "graphical-session.target";
    events = [
      {
        event = "lock";
        command = "${lib.getExe config.programs.swaylock.package} -f";
      }
    ];
    timeouts = [
      {
        timeout = idle-timeout;
        command = write-idle.outPath;
        resumeCommand = write-active.outPath;
      }
      {
        timeout = 600;
        command = "${lib.getBin pkgs.systemd}/bin/systemctl suspend";
      }
    ];
  };
  systemd.user.services.swayidle = {
    # Restart = "always"; set in the module
    Service.RestartSec = "3s";
    Unit.StartLimitIntervalSec = "18s";
  };
}
