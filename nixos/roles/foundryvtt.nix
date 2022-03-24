{pkgs, ...}: let
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
in {
  config = {
    systemd.services."${name}" = {
      wantedBy = ["multi-user.target"];
      description = "Foundryvtt server";
      preStart = ''
        mkdir -p ${dataDir}
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
      serviceConfig = {
        StateDirectory = "${name}";
        WorkingDirectory = stateDir;
        DynamicUser = true;
        Restart = "always";
        Environment = "HOME=${stateDir}";
        ExecStart = "${pkgs.nodejs}/bin/node ${stateDir}/app/resources/app/main.js --dataPath=\"${dataDir}\"";
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
              #"/rules/" = {
              #  alias = "${pkgs.fetchzip (import ./5etools-url.nix // {stripRoot = false;})}/";
              #  index = "index.html";
              #};
              "/" = {
                proxyPass = "http://[::1]:${toString config.port}";
                proxyWebsockets = true;
                extraConfig =
                  #if ($request_uri ~ ^/rules$) {
                  #   return 301 /rules/;
                  #}
                  ''
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
  };
}
