{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (config.m-0) virtualHosts;
in
{
  environment.persistence.snapshoted.directories = [
    "/var/www/5etools"
    "/var/www/5etools-2014"
  ];

  services.nginx.virtualHosts.${virtualHosts."5e"}.locations."/".root = "/var/www/5etools";
  services.nginx.virtualHosts.${virtualHosts."5e-2014"}.locations."/".root = "/var/www/5etools-2014";

  systemd.services.update-5etools = {
    script = ''
      cd /var/www/5etools
      ${lib.getExe pkgs.git} pull -r
      cd img
      ${lib.getExe pkgs.git} pull
      cd /var/www/5etools-2014
      ${lib.getExe pkgs.git} pull -r
      cd img
      ${lib.getExe pkgs.git} pull
    '';
    startAt = "daily";
  };
}
