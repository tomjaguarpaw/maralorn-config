{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      usbutils
      pciutils
      htop
      pamtester
      sqlite-interactive
      nethogs
      ;
  };
  programs.mtr.enable = true;
}
