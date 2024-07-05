{
  pkgs,
  lib,
  config,
  ...
}:
let
  openbar = name: outputs: {
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
      (profile "desk" (
        openbar "big" [
          {
            criteria = "Dell Inc. DELL S2721QS F9SJM43";
            status = "enable";
            scale = 1.0;
          }
        ]
      ))
      (profile "docked" (
        openbar "big" [
          {
            criteria = "Dell Inc. DELL S2721QS F9SJM43";
            status = "enable";
            scale = 1.0;
          }
          {
            criteria = "eDP-1";
            status = "disable";
          }
        ]
      ))
      (profile "docked-s" (
        openbar "big" [
          {
            criteria = "Dell Inc. DELL S2721QS 77SJM43";
            status = "enable";
            scale = 1.0;
          }
          {
            criteria = "eDP-1";
            status = "disable";
          }
        ]
      ))
      (profile "tv" {
        outputs = [
          {
            criteria = "*";
            status = "disable";
          }
          {
            criteria = "Panasonic Industry Company Panasonic-TV 0x00000101";
            status = "enable";
            mode = "1920x1080";
          }
        ];
      })
      (profile "default-single" (
        openbar "small" [
          {
            criteria = "eDP-1";
            status = "enable";
            scale = 1.0;
          }
        ]
      ))
      (profile "default-extern" (
        openbar "small" [
          {
            criteria = "eDP-1";
            status = "enable";
            scale = 1.0;
          }
          {
            criteria = "*";
            status = "enable";
          }
        ]
      ))
    ];
  };
  systemd.user.services.kanshi = {
    Service.RestartSec = "3s";
    Unit.StartLimitIntervalSec = "18s";
  };
}
