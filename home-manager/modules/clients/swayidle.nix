{ pkgs, lib, ... }:
let
  write-idle = pkgs.writeShellScript "write-idle" ''
    echo "{\"at\": $(${
      lib.getBin pkgs.coreutils
    }/bin/date +%s), \"idle\": $1}" > ~/.idle_state
  '';
in
{
  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 1;
        command = "${write-idle} true";
        resumeCommand = "${write-idle} false";
      }
      {
        timeout = 300;
        command = "${lib.getBin pkgs.hyprland}/bin/hyprctl dispatch dpms off";
        resumeCommand = "${lib.getBin pkgs.hyprland}/bin/hyprctl dispatch dpms on";
      }
      {
        timeout = 600;
        command = "${lib.getBin pkgs.systemd}/bin/systemctl suspend";
      }
    ];
  };
}
