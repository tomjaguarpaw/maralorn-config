{
  pkgs,
  lib,
  config,
  ...
}:
let
  openbar = outputs: {
    inherit outputs;
    exec = [
      (pkgs.writeShellScript "launch-eww" ''
        if [[ "$(${lib.getExe config.programs.eww.package} ping)" != "pong" ]]; then
          ${lib.getExe' pkgs.systemd "systemctl"} --user restart eww
          sleep 0.1s;
        fi
        ${lib.getExe config.programs.eww.package} open topbar
      '').outPath
    ];
  };
  profile = name: config: {
    profile = {
      inherit name;
    } // config;
  };
in
{
  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";
    settings = [
      (profile "desk" (openbar [
        {
          criteria = "Dell Inc. DELL S2721QS F9SJM43";
          status = "enable";
          scale = 1.0;
        }
      ]))
      (profile "docked" (openbar [
        {
          criteria = "Dell Inc. DELL S2721QS F9SJM43";
          status = "enable";
          scale = 1.0;
        }
        {
          criteria = "eDP-1";
          status = "disable";
        }
      ]))
      (profile "docked-s" (openbar [
        {
          criteria = "Dell Inc. DELL S2721QS 77SJM43";
          status = "enable";
          scale = 1.0;
        }
        {
          criteria = "eDP-1";
          status = "disable";
        }
      ]))
      (profile "tv" (openbar [
        {
          criteria = "*";
          status = "disable";
        }
        {
          criteria = "Panasonic Industry Company Panasonic-TV 0x01010101";
          status = "enable";
          mode = "1920x1080";
          scale = 1.0;
        }
      ]))
      (profile "default-single" (openbar [
        {
          criteria = "eDP-1";
          status = "enable";
          scale = 1.0;
        }
      ]))
      (profile "default-extern" (openbar [
        {
          criteria = "eDP-1";
          status = "enable";
          scale = 1.0;
        }
        {
          criteria = "*";
          status = "enable";
          scale = 1.0;
        }
      ]))
    ];
  };
  systemd.user.services.kanshi = {
    Service.RestartSec = "3s";
    Unit.StartLimitIntervalSec = "18s";
  };
}
