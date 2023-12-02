{pkgs, ...}:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      usbutils
      pciutils
      htop
      pamtester
      sqlite-interactive
      ;
  };
  programs.mtr.enable = true;
}
