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
        fi
        ${lib.getExe config.programs.eww.package} open ${name}
      '').outPath
    ];
  };
in
{
  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";
    profiles = {
      desk = openbar "middlebar" [
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
          status = "enable";
        }
      ];
      docked = openbar "middlebar" [
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
      docked-s = openbar "middlebar" [
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
      z-default-single = openbar "rightbar" [
        {
          criteria = "eDP-1";
          status = "enable";
        }
      ];
      z-default-extern = openbar "rightbar" [
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
