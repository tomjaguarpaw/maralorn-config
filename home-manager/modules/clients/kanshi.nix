{ lib, ... }:
let
  inject_exec = lib.mapAttrs (
    _: x:
    x
    // {
      exec = [
        "eww open bar"
        "eww open bg"
      ];
    }
  );
in
{
  services.kanshi = {
    enable = true;
    profiles = inject_exec {
      undocked.outputs = [ { criteria = "eDP-1"; } ];
      docked.outputs = [
        {
          criteria = "eDP-1";
          status = "disable";
        }
        {
          criteria = "Dell Inc. Dell U4919DW 2RSQXH3";
          mode = "5120x1440";
        }
      ];
    };
  };
}
