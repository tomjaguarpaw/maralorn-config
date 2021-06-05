{ pkgs, lib, config, ... }:
let
  name = "foundryvtt";
  stateDir = "/var/lib/${name}";
  dataDir = "${stateDir}/data";
  configFile = "${dataDir}/Config/options.json";
  config = {
    port = 3333;
    upnp = false;
    hostname = "rpg.maralorn.de";
    routePrefix = null;
    proxySSL = true;
    proxyPort = null;
    minifyStaticFiles = true;
    updateChannel = "release";
  };
  declarativeConfigFile = builtins.toFile "foundry-options.json" (builtins.toJSON config);
in
{
  config = {
    systemd.services.${name} = {
      enable = true;
      description = "Foundryvtt server";
      serviceConfig = {
        StateDirectory = "${name}";
        WorkingDirectory = stateDir;
        DynamicUser = true;
        Restart = "always";
        ExecStartPre = pkgs.writeShellScript "setup-foundry-vtt" ''
          uid
          mkdir -p ${stateDir}/app
          if [[ -f "${configFile}" ]]; then
            tempfile=$(mktemp)
            cp "${configFile}" "$tempfile"
            ${pkgs.jq}/bin/jq ".[0] * .[1]" -s "$tempfile" "${declarativeConfigFile}" > "${configFile}"
          else
            cp "${declarativeConfigFile}" "${configFile}"
          fi
          if [[ ! -f "${stateDir}/app/resources/app/main.js" ]]; then
            echo "No ${name} app found. Please download zip from foundryvtt.com and extract to ${stateDir}/app"
          fi
        '';
        ExecStart = "${pkgs.nodejs}/bin/node ${stateDir}/app/resources/app/main.js --dataPath=${stateDir}/data";
      };
    };
    services = {
      nginx = {
        virtualHosts = {
          ${config.hostname} = {
            extraConfig = ''
              client_max_body_size 300M;
              proxy_set_header Host $host;
            '';
            forceSSL = true;
            enableACME = true;
            locations."/" = {
              proxyPass = "http://[::1]:${toString config.port}";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_set_header Host $host;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
              '';
            };
          };
        };
      };
    };
  };
}
