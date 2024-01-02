{ pkgs, lib, ... }:
let
  inject_exec = lib.mapAttrs (
    lib.const (
      lib.recursiveUpdate { exec = [ "${lib.getExe' pkgs.systemd "systemctl"} --user restart eww" ]; }
    )
  );
in
{
  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";
    profiles = inject_exec {
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
      desk.outputs = [
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
          status = "enable";
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
      z-default-single.outputs = [
        {
          criteria = "eDP-1";
          status = "enable";
        }
      ];
      z-default-extern.outputs = [
        {
          criteria = "eDP-1";
          status = "disable";
        }
        {
          criteria = "*";
          status = "enable";
        }
      ];
    };
  };
}
