{
  config,
  lib,
  pkgs,
  ...
}:
let
  name = "five-e-tools";
  stateDir = "/var/lib/${name}";
  inherit (config.m-0) virtualHosts;
in
{
  environment.persistence.snapshoted.directories = [ "/var/lib/private/${name}" ];
  systemd.services = {

    "restart-${name}" = {
      description = "Restart ${name}";
      serviceConfig.ExecStart = "${lib.getExe' pkgs.systemd "systemctl"} restart ${name}";
      startAt = "daily";
    };

    ${name} = {
      wantedBy = [ "multi-user.target" ];
      description = "5etools server";
      path = [
        pkgs.coreutils
        pkgs.nodejs
        pkgs.bash
        pkgs.git
      ];
      preStart = ''
        if [[ ! -d ".git" ]]; then
          echo "No ${name} app found. Please checkout mirror and img/ folder."
          exit 1
        fi
        git pull -r
        cd img
        git pull
        cd ..
        npm run build:sw:prod
      '';
      serviceConfig = {
        StateDirectory = "${name}";
        WorkingDirectory = stateDir;
        DynamicUser = true;
        Restart = "always";
        ExecStart = "${lib.getExe' pkgs.nodejs "npm"} run serve:dev";
      };
    };
  };
  services.nginx.virtualHosts."${virtualHosts."5e"}".locations."/" = {
    proxyPass = "http://127.0.0.1:${toString 5000}";
  };

}
