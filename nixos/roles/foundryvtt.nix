{ pkgs, lib, config, ... }:
let
  name = "foundryvtt";
  stateDir = "/var/lib/${name}";
  port = "3333";
in {
  config = {
    users = {
      groups.${name} = { };
      users.${name} = {
        group = name;
        home = stateDir;
      };
    };
    systemd.services.${name} = {
      enable = true;
      description = "Foundryvtt server";
      serviceConfig = {
        WorkingDirectory = stateDir;
        #ExecStartPre = pkgs.writeShellScript "setup-foundry-vtt" ''
        #mkdir -p ${stateDir}/app ${stateDir}/data
        #if [[ -f "${stateDir}/${name}.zip" ]]; then
        #${pkgs.coreutils}/bin/rm -rf app
        #mkdir -p app
        #cd app
        #${pkgs.unzip}/bin/unzip ${stateDir}/${name}.zip
        #${pkgs.coreutils}/bin/mv ${stateDir}/${name}.zip ${stateDir}/${name}.zip
        #else
        #if [[ ! -f "${stateDir}/app/resources/app/main.js" ]]; then
        #echo "No ${name} app found. Please download zip from foundryvtt.com and place at ${stateDir}/${name}.zip"
        #fi
        #fi
        #'';
        ExecStart =
          "${pkgs.nodejs}/bin/node ${stateDir}/app/resources/app/main.js --port=${port} --dataPath=${stateDir}/data";
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
