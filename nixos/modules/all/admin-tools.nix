{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs) usbtools pciutils htop;
  };
}
