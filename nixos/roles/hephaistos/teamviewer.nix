{ pkgs, lib, ... }:
{
  services.teamviewer.enable = true;
  systemd.services.teamviewerd.serviceConfig.Environment = "PATH=${
    lib.makeBinPath [ pkgs.coreutils ]
  }:/run/wrappers/bin";
}
