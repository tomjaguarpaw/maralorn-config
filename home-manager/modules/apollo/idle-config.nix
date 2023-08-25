{ pkgs, lib, ... }:
let
  command = "${lib.getExe pkgs.swaylock} -f";
in
{
  services.swayidle = {
    events = [
      {
        event = "before-sleep";
        inherit command;
      }
      {
        event = "lock";
        inherit command;
      }
    ];
    timeouts = [ {
      timeout = 300;
      inherit command;
    } ];
  };
}
