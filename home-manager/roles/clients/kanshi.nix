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
        ${lib.getExe config.programs.eww.package} open topbar --arg screen=${name}
      '').outPath
    ];
  };
in
{
  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";
    profiles = {
      desk = openbar "big" [
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
          status = "enable";
        }
      ];
      docked = openbar "big" [
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
          status = "enable";
        }
        {
          criteria = "eDP-1";
          status = "disable";
        }
      ];
      docked-s = openbar "big" [
        {
          criteria = "Dell Inc. DELL S2721QS 77SJM43";
          status = "enable";
        }
        {
          criteria = "eDP-1";
          status = "disable";
        }
      ];
      tv.outputs = [
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
      z-default-single = openbar "small" [
        {
          criteria = "eDP-1";
          status = "enable";
        }
      ];
      z-default-extern = openbar "small" [
        {
          criteria = "eDP-1";
          status = "enable";
        }
        {
          criteria = "*";
          status = "enable";
        }
      ];
    };
  };
  systemd.user.services.kanshi.Service.RestartSec = "3s";
}
