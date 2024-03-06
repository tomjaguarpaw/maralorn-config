{ pkgs, lib, ... }:
let
  mkMonitorline = action: monitor: ''
    ACTION=="${action}", ENV{DEVTYPE}=="usb_device", ENV{PRODUCT}=="3496/6/100", RUN+="${lib.getExe pkgs.ddcutil} -l \"DELL S2721QS\" setvcp 60 ${monitor}"
  '';
in
{

  services.udev.extraRules = lib.concatStringsSep "\n" [
    (mkMonitorline "add" "15") # DP
    (mkMonitorline "remove" "17") # HDM11
  ];
}
