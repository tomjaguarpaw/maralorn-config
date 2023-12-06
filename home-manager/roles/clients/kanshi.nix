{
  pkgs,
  lib,
  mylib,
  ...
}:
let
  inject_exec = lib.mapAttrs (
    lib.const (
      lib.recursiveUpdate {exec = ["${mylib.getExe' pkgs.systemd "systemctl"} --user restart eww"];}
    )
  );
in
{
  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";
    profiles = inject_exec {
      undocked.outputs = [
        {
          criteria = "eDP-1";
          status = "enable";
        }
      ];
      docked.outputs = [
        {
          criteria = "eDP-1";
          status = "disable";
        }
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
          status = "enable";
        }
      ];
      office.outputs = [
        {
          criteria = "eDP-1";
          status = "disable";
        }
        {
          criteria = "Dell Inc. DELL S2721QS F9SJM43";
          status = "enable";
        }
      ];
      desk.outputs = [
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
          status = "enable";
        }
      ];
      tv.outputs = [
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          status = "disable";
        }
        {
          criteria = "Panasonic Industry Company Panasonic-TV 0x00000101";
          status = "enable";
          mode = "1920x1080";
        }
      ];
    };
  };
}
