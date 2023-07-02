{ config, pkgs, lib, ... }:
let inherit (config.m-0) virtualHosts;
in {
  environment.persistence."/disk/persist".directories = [ "/var/www/5etools" ];

  services.nginx.virtualHosts.${virtualHosts."5e"}.locations."/".root =
    "/var/www/5etools";

  systemd.services.update-5etools = {
    script = ''
      cd /var/www/5etools
      git config --global user.email "mail@maralorn.de"
      git config --global user.name "maralorn"
      if [[ -d ".git" ]]; then
         ${lib.getExe pkgs.git} pull -r
      else
         ${
           lib.getExe pkgs.git
         } clone https://github.com/5etools-mirror-1/5etools-mirror-1.github.io.git .
      fi
    '';
    startAt = "daily";
  };
}
