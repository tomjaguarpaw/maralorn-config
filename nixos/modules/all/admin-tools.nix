{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      usbutils
      pciutils
      htop
      pamtester
    ;
  };
  programs.mtr.enable = true;
}
