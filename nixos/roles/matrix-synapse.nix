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
            return 200 "{\"m.homeserver\": \"matrix.maralorn.de:443\"}";
          '';
        };
        extraConfig = "
          add_header 'Access-Control-Allow-Origin' '*';
          add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS';
          add_header 'Access-Control-Allow-Headers' 'Origin, X-Requested-With, Content-Type, Accept, Authorization';
        ";
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
        extraConfig = "
          add_header 'Access-Control-Allow-Origin' '*';
          add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS';
          add_header 'Access-Control-Allow-Headers' 'Origin, X-Requested-With, Content-Type, Accept, Authorization';
        ";
      };
    };

    # Postgres
    postgresql.enable = true;

    # Synapse
    matrix-synapse = {
      enable = true;
      package = pkgs.matrix-synapse;
      enable_metrics = true;
      inherit server_name;
      public_baseurl = "https://${hostName}";
      url_preview_enabled = true;
      database_type = "psycopg2";
      max_upload_size = "30M";
      dynamic_thumbnails = true;
      registration_shared_secret =
        config.m-0.private.matrix_registration_secret;
      macaroon_secret_key = config.m-0.private.macaroon_secret;
      turn_uris = [ "turn:hera.m-0.eu:3478?transport=udp" ];
      turn_shared_secret = config.m-0.private.turn_secret;
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
