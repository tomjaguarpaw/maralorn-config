{ pkgs, lib, ... }:
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
    systemd.services."${name}" = {
      wantedBy = [ "multi-user.target" ];
      description = "Foundryvtt server";
      preStart = ''
        mkdir -p ${dataDir}
        if [[ -f "${configFile}" ]]; then
          tempfile=$(mktemp)
          cp "${configFile}" "$tempfile"
          ${lib.getExe pkgs.jq} ".[0] * .[1]" -s "$tempfile" "${declarativeConfigFile}" > "${configFile}"
        else
          cp "${declarativeConfigFile}" "${configFile}"
        fi
        if [[ ! -f "${stateDir}/app/resources/app/main.js" ]]; then
          echo "No ${name} app found. Please download zip from foundryvtt.com and extract to ${stateDir}/app"
        fi
      '';
      serviceConfig = {
        StateDirectory = "${name}";
        WorkingDirectory = stateDir;
        DynamicUser = true;
        Restart = "always";
        Environment = "HOME=${stateDir}";
        ExecStart = ''${lib.getExe pkgs.nodejs} ${stateDir}/app/resources/app/main.js --dataPath="${dataDir}"'';
      };
    };
    services = {
      nginx = {
        virtualHosts = {
          "${config.hostname}" = {
            extraConfig = ''
              client_max_body_size 300M;
              proxy_set_header Host $host;
            '';
            forceSSL = true;
            enableACME = true;
            locations = {
              "/" = {
                proxyPass = "http://[::1]:${toString config.port}";
                proxyWebsockets = true;
                extraConfig = ''
                  proxy_set_header Host $host;
                  proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                  proxy_set_header X-Forwarded-Proto $scheme;
                  if ($query_string ~ "pw=([A-Za-z]*)") {
                     add_header Set-Cookie "password=$1; path=/; Max-Age=${toString (365 * 24 * 60 * 60)}; Secure";
                     return 303 /;
                  }
                  if ($http_cookie !~ "password=${pkgs.privateValue "" "foundry-pw"}") {
                     return 303 /logout;
                  }
                '';
              };
              "/login".extraConfig = ''
                more_set_headers 'Content-Type: text/html';
                return 200 '<!DOCTYPE html><html><head><meta charset="UTF-8"></head><body><form style="text-align: center; margin: 250px auto; width: 500px;" action="/" method="get"><label for="pw">Passwort: </label><input type="password" name="pw"><input type="submit" value="login"></form></html></body>';
              '';
              "/logout".extraConfig = ''
                add_header Set-Cookie 'password=""; Max-Age=0';
                return 303 /login;
              '';
            };
          };
        };
      };
    };
  };
}
