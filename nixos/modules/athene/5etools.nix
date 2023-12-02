{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (config.m-0) virtualHosts;
in
{
  environment.persistence.snapshoted.directories = ["/var/www/5etools"];

  services.nginx.virtualHosts.${virtualHosts."5e"}.locations."/".root = "/var/www/5etools";

  systemd.services.update-5etools = {
    script = ''
      cd /var/www/5etools
      ${lib.getExe pkgs.git} config user.email "mail@maralorn.de"
      ${lib.getExe pkgs.git} config user.name "maralorn"
      if [[ -d ".git" ]]; then
         ${lib.getExe pkgs.git} pull -r
      else
         ${lib.getExe pkgs.git} clone https://github.com/5etools-mirror-1/5etools-mirror-1.github.io.git .
      fi
    '';
    startAt = "daily";
  };
}
