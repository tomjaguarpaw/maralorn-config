{ pkgs, ... }:
{

  hardware.opengl = {
    #extraPackages = [ pkgs.amdvlk ];
    #extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
    enable = true;
    driSupport = true;
    driSupport32Bit = true; # for gw2
  };

  programs.steam.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;
}
