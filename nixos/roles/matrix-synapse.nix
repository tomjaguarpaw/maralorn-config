{ pkgs, config, ... }:
let
  server_name = "maralorn.de";
  hostName = "matrix.${server_name}";
in {
  services = {
    nginx = {
      enable = true;
      virtualHosts."${server_name}" = {
        enableACME = true;
        forceSSL = true;
        locations = {
          "/.well-known/matrix/server".extraConfig = ''
            default_type application/json;
            return 200 "{\"m.server\": \"matrix.maralorn.de:443\"}";
          '';
          "/.well-known/matrix/client".extraConfig = ''
            default_type application/json;
            return 200 "{\"m.homeserver\": { \"base_url\":\"https://matrix.maralorn.de\"} }";
          '';
        };
        extraConfig =
          "\n          add_header 'Access-Control-Allow-Origin' '*';\n          add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS';\n          add_header 'Access-Control-Allow-Headers' 'Origin, X-Requested-With, Content-Type, Accept, Authorization';\n        ";
      };
      virtualHosts."${hostName}" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            proxyPass = "http://[::1]:8008";
            extraConfig = "proxy_set_header X-Forwarded-For $remote_addr;";
          };
        };
      };
    };

    # Postgres
    postgresql.enable = true;

    # Synapse
    matrix-synapse = let
      server-secrets = pkgs.privateValue {
        registration_shared_secret = "";
        macaroon_secret_key = "";
        turn_shared_secret = "";
      } "matrix/server-secrets";
    in server-secrets // {
      enable = true;
      package = pkgs.matrix-synapse;
      enable_metrics = true;
      inherit server_name;
      public_baseurl = "https://${hostName}";
      url_preview_enabled = true;
      database_type = "psycopg2";
      max_upload_size = "30M";
      dynamic_thumbnails = true;
      turn_uris = [ "turn:hera.m-0.eu:3478?transport=udp" ];
      turn_user_lifetime = "5h";
      allow_guest_access = true;
      logConfig = ''
        version: 1

        formatters:
          journal_fmt:
            format: '%(name)s: [%(request)s] %(message)s'

        filters:
          context:
            (): synapse.util.logcontext.LoggingContextFilter
            request: ""

        handlers:
          journal:
            class: systemd.journal.JournalHandler
            formatter: journal_fmt
            filters: [context]
            SYSLOG_IDENTIFIER: synapse

        disable_existing_loggers: True

        loggers:
          synapse:
            level: WARN
          synapse.storage.SQL:
            level: WARN

        root:
          level: WARN
          handlers: [journal]
      '';
      database_args = {
        user = "matrix-synapse";
        database = "matrix-synapse";
        cp_min = 5;
        cp_max = 10;
      };
      report_stats = true;
      listeners = [
        {
          type = "metrics";
          port = 9148;
          bind_address = "127.0.0.1";
          resources = [ ];
          tls = false;
        }
        {
          port = 8008;
          bind_address = "::1";
          resources = [
            {
              compress = false;
              names = [ "client" ];
            }
            {
              compress = false;
              names = [ "federation" ];
            }
          ];
          x_forwarded = true;
          tls = false;
        }
      ];
    };
  };
}
