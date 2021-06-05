{ pkgs, lib, config, ... }:
let
  name = "foundryvtt";
  stateDir = "/var/lib/${name}";
  port = "3333";
in
{
  config = {
    users = {
      groups.${name} = { };
      users.${name} = {
        group = name;
        home = stateDir;
        isSystemUser = true;
      };
    };
    systemd.services.${name} = {
      enable = true;
      description = "Foundryvtt server";
      serviceConfig = {
        WorkingDirectory = stateDir;
        Restart = "always";
        # Ensure ./app and ./data exist and extract the game zip to ./app
        ExecStart = "${pkgs.nodejs}/bin/node ${stateDir}/app/resources/app/main.js --port=${port} --dataPath=${stateDir}/data";
        User = name;
      };
    };
    services = {
      nginx = {
        virtualHosts = {
          "rpg.maralorn.de" = {
            extraConfig = ''
              client_max_body_size 300M;
            '';
            forceSSL = true;
            enableACME = true;
            locations."/" = {
              proxyPass = "http://[::1]:${port}";
              proxyWebsockets = true;
            };
          };
        };
      };
    };
  };
}
