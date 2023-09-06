{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs) usbutils pciutils htop;
  };
}
