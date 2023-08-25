{ pkgs, lib, ... }:
{
  services.swayidle = {
    events = [
      {
        event = "before-sleep";
        command = lib.getExe pkgs.swaylock;
      }
      {
        event = "lock";
        command = lib.getExe pkgs.swaylock;
      }
    ];
    timeouts = [ {
      timeout = 300;
      command = lib.getExe pkgs.swaylock;
    } ];
  };
}
