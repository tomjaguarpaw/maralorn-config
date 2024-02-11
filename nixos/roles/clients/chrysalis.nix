{ pkgs, ... }:
{
  services.udev.packages = [ pkgs.chrysalis ];
}
